package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min


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

  property("insert two elements into an empty heap, findMin should be min of two elems") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    Math.min(a, b) == findMin(h)
  }

  property("insert element into empty heap, when deleted its minimum, resulting heap should be empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("any heap, you should get a sorted sequence of elements when continually finding and deleting minima") = forAll { (h: H) =>
    def allElemsMoreThan(h: H, prevMin: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val currMin = findMin(h)
        (currMin >= prevMin) && allElemsMoreThan(deleteMin(h), currMin)
      }
    }
    allElemsMoreThan(h, Int.MinValue)
  }

  property("a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("adding an elem to the empty, when deleting the min should return empty again") = forAll { (a: Int) =>
    def h = insert(a, empty)

    findMin(h) == a && deleteMin(h) == empty
  }

  property("melding empty and empty should return empty") = forAll { (a: Int) =>
    meld(empty, empty) == empty
  }

  property("Recusivly finding and deleting elements in a Heap should return same elements") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("melding two heaps is equal to moving first min to second and removing it from min") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    val bothMelded = meld(h1, h2)
    val meldedAfterMovingMin = meld(deleteMin(h1), insert(findMin(h1), h2))
    heapEqual(bothMelded, meldedAfterMovingMin)
  }

}
