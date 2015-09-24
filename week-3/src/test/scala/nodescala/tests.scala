package nodescala

import java.util.NoSuchElementException

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  test("Future.all should complete with list of values") {
    val successAll = Future.all(List(Future.always(1), Future.always(2)))
    assert(Await.result(successAll, 1 second) == List(1, 2))
  }
  test("Future.all should never be completed") {
    val failedAll = Future.all(List(Future.always(1), Future.never))
    try {
      Await.result(failedAll, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  test("Future.any should complete with first value") {
    val successAny = Future.any(List(Future.always(1), Future.never))
    assert(Await.result(successAny, 1 second) == 1)
  }
  test("Future.any should never be completed") {
    val failedAny = Future.any(List(Future.never, Future.always(1)))
    try {
      Await.result(failedAny, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.always(x).now should return value") {
    assert(Future.always(1).now === 1)
  }
  test("Future.never.now should throw exception") {
    try {
      Future.never.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }
  test("Future.delay(1 second).now should throw exception") {
    try {
      Future.delay(1 second).now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }

  test("Future.always(1).continueWith(_ + 1) should return Future(2)") {
    val f = Future.always(1).continueWith(_.now + 1)
    assert(Await.result(f, 1 second) === 2)
  }
  test("Future.always(1).continue(_ + 1) should return Future(2)") {
    val f = Future.always(1).continue(_.getOrElse(0) + 1)
    assert(Await.result(f, 1 second) === 2)
  }
  
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




