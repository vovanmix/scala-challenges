package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(!contains(s3, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val bigSet = union(union(s1, s2), s3) // 1, 2, 3
      assert(contains(bigSet, 1), "")
      assert(contains(bigSet, 2), "")
      assert(contains(bigSet, 3), "")
      assert(!contains(bigSet, 4), "")
    }
  }

  test("intersect") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "")
      assert(!contains(s, 2), "")
      val t = intersect(s1, s1)
      assert(contains(t, 1), "")
      assert(!contains(t, 2), "")

      val bigSet1 = union(union(s1, s2), s3) // 1, 2, 3
      val bigSet2 = union(union(s2, s3), s4) // 2, 3, 4
      val bigIntersect = intersect(bigSet1, bigSet2) // 2, 3
      assert(!contains(bigIntersect, 1), "")
      assert(contains(bigIntersect, 2), "")
      assert(contains(bigIntersect, 3), "")
      assert(!contains(bigIntersect, 4), "")
    }
  }

  test("diff") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "")
      assert(!contains(s, 2), "")
      val t = diff(s1, s1)
      assert(!contains(t, 1), "")
      assert(!contains(t, 2), "")

      val bigSet1 = union(union(s1, s2), s3) // 1, 2, 3
      val bigSet2 = union(union(s2, s3), s4) // 2, 3, 4
      val bigDiff = diff(bigSet1, bigSet2) // 1
      assert(contains(bigDiff, 1), "")
      assert(!contains(bigDiff, 2), "")
      assert(!contains(bigDiff, 3), "")
      assert(!contains(bigDiff, 4), "")
    }
  }

  test("filter") {
    new TestSets {
      val bigSet = union(union(s2, s3), s4) // 2, 3, 4
      val filtered = filter(bigSet, _ > 2)

      assert(!contains(filtered, 1), "")
      assert(!contains(filtered, 2), "")
      assert(contains(filtered, 3), "")
      assert(contains(filtered, 4), "")
    }
  }

  test("forall") {
    new TestSets {
      val bigSet = union(union(s2, s3), s4) // 2, 3, 4

      assert(forall(bigSet, _ > 0))
      assert(forall(bigSet, math.pow(_, 2) < 100))
      assert(!forall(bigSet, _ > 2))

//      val diffSet = diff(bigSet, bigSet) // empty
//      assert(!forall(diffSet, _ > 0))
    }
  }

  test("exists") {
    new TestSets {
      val bigSet = union(union(s2, s3), s4) // 2, 3, 4
      assert(exists(bigSet, _ > 2))
      assert(!exists(bigSet, _ < 2))
      assert(exists(bigSet, math.pow(_, 2) > 9))
      assert(!exists(bigSet, math.pow(_, 2) > 100))
    }
  }

  test("map") {
    new TestSets {
      val bigSet = union(union(s2, s3), s4) // 2, 3, 4
      var mapped = map(bigSet, _ * 2)
      assert(contains(mapped, 4))
      assert(contains(mapped, 6))
      assert(contains(mapped, 8))
      assert(!contains(mapped, 2))
      assert(!contains(mapped, 3))
    }
  }
}
