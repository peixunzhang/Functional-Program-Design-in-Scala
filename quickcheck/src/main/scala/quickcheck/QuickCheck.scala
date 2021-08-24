package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{forAll, propBoolean}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.listOf(arbitrary[Int]).map { xs =>
    xs.foldRight(empty)(insert)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (v1: Int, v2: Int) =>
    findMin(insert(v2, insert(v1, empty))) == v1.min(v2)
  }

  property("gen3") = forAll { (v1: Int) =>
    isEmpty(deleteMin(insert(v1, empty)))
  }

  property("gen4") = {
    def go(h: H): List[A] =
      if (isEmpty(h)) Nil else findMin(h) :: go(deleteMin(h))
    forAll { (h: H) =>
      go(h) == go(h).sorted
    }
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      findMin(meld(h1, h2)) == (findMin(h1)).min(findMin(h2))
    }
  }
}
