package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //  If you insert any two elements into an empty heap, finding the minimum of the
  //  resulting heap should get the smallest of the two elements back.
  property("inserting two elements") = forAll { t: (Int, Int) =>
    val h1 = insert(t._1, empty)
    val h2 = insert(t._2, h1)
    findMin(h2) == math.min(t._1, t._2)
    val h3 = deleteMin(h2)
    findMin(h3) == math.max(t._1, t._2)
  }

  //  If you insert an element into an empty heap, then delete the minimum, the resulting
  //  heap should be empty.
  property("inserting one and deleting") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  //  Given any heap, you should get a sorted sequence of elements when continually
  //  finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("continually finding and deleting minima") = forAll { heap: H =>
    def findAndDelete(h: H): List[Int] =
      if(isEmpty(h)) List() else findMin(h) :: findAndDelete(deleteMin(h))

    val list = findAndDelete(heap)
    list == list.sortWith(_<_)
  }

  property("create a heap and then continually finding and deleting minima") = forAll { l: List[Int] =>
    def findAndDelete(h: H): List[Int] =
      if(isEmpty(h)) List() else findMin(h) :: findAndDelete(deleteMin(h))

    def makeHeap(li: List[Int], h: H): H = li match {
      case List() => h
      case x :: xs => makeHeap(xs, insert(x, h))
    }

    val heap = makeHeap(l, empty)
    val list = findAndDelete(heap)
    list == l.sortWith(_<_)
  }

  //  Finding a minimum of the melding of any two heaps should return a
  //  minimum of one or the other.
  property("Finding a minimum of the melding of two heaps") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == math.min(findMin(h1), findMin(h2))
  }
}
