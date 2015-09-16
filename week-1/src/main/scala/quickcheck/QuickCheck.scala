package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("minOfOne") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOfTwo") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(h) == List(a, b, c).min
  }

  property("insertToEmptyAndDelee") = forAll { (a: Int, b: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("minAfterMeld") = forAll { (h: H, i: H) =>
    val j = meld(h, i)
    findMin(j) == List(findMin(h), findMin(i)).min
  }

  property("meldWithEmpty") = forAll { (h: H) =>
    val i = meld(h, empty)
    val j = meld(empty, h)
    h == i && h == j
  }

  def deconstructHeap(h: H): List[Int] = {
    def loop(acc: List[Int], i: H): List[Int] = {
      if (isEmpty(i)) acc
      else loop(findMin(i) :: acc, deleteMin(i))
    }
    loop(Nil, h).reverse
  }

  def reconstructHeap(xs: List[Int]): H = {
    def loop(acc: H, ys: List[Int]): H = {
      if (ys.isEmpty) acc
      else loop(insert(ys.head, acc), ys.tail)
    }
    loop(empty, xs)
  }

  property("deconstructingHeapToSortedList") = forAll { (h: H) =>
    val list = deconstructHeap(h)
    list == list.sorted
  }

  property("deconstructingMeldedHeapsToSortedList") = forAll { (h: H, i: H) =>
    val list = deconstructHeap(meld(h, i))
    list == list.sorted
  }

  property("deconstructingAndReconstructingHeap") = forAll { (h: H) =>
    val list = deconstructHeap(h)
    val i = reconstructHeap(list)
    deconstructHeap(h) == deconstructHeap(i)
  }
}
