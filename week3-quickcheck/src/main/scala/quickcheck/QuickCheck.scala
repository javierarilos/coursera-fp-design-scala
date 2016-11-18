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
    deleteMin(h) == empty
  }

  property("any heap, you should get a sorted sequence of elements when continually finding and deleting minima") = forAll { (h: H) =>
    def allElemsLessThan(h: H, prevMin: Int): Boolean = {
      val min = findMin(h)
      if (min >= prevMin)
        allElemsLessThan(deleteMin(h), min)
      else
        false
    }
    allElemsLessThan(h, Int.MaxValue)
  }

}
