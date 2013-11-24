package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("after deleting one element from a one element heap, it is empty") =
    forAll{ a: Int =>
      val h = insert(a, empty)
      val h2 = deleteMin(h)
      isEmpty(h2)
    }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }


  property("delete") = forAll { (a: Int, b :Int, c :Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    val ordered = List(a,b,c).sorted
    val m = findMin(h)
    m == ordered(0)
    val m2 = findMin(deleteMin(h))
    m2 == ordered(1)
    val m3 = findMin(deleteMin(deleteMin(h)))
    m3 == ordered(2)
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
