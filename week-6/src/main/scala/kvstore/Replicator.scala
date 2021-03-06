package kvstore

import akka.actor.{ReceiveTimeout, Props, Actor, ActorRef}
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  context.setReceiveTimeout(200.millis)

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case request: Replicate => {
      val seq = nextSeq
      acks += seq ->(sender, request)
      val snapshot = Snapshot(request.key, request.valueOption, seq)
      replica ! snapshot
      context.become(replicating(snapshot))
    }
  }

  def replicating(snapshot: Snapshot): Receive = {
    case Replicate(key: String, valueOption: Option[String], id: Long) => {
      pending :+= Snapshot(key, valueOption, nextSeq)
    }
    case SnapshotAck(key: String, seq: Long) => {
      acks.get(seq) match {
        case Some((sender, request)) => {
          sender ! Replicated(key, request.id)

          if (pending.isEmpty) context.become(receive)
          else pending match {
            case firstPending +: ps => {
              pending = ps
              context.become(replicating(firstPending))
            }
          }
        }
        case None => throw new RuntimeException("Should not happen")
      }
    }
    case ReceiveTimeout => replica ! snapshot
  }

}
