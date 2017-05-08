package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  def initializeStackOverflow(): Boolean =
    try {
      StackOverflow
      true
    } catch {
      case _: Throwable => false
    }


  override def afterAll(): Unit = {
    assert(initializeStackOverflow(), " -- did you fill in all the values in StackOverflow (conf, sc, wikiRdd)?")
    import StackOverflow._
    sc.stop()
  }


  test("groupedPostings") {
    assert(initializeStackOverflow(), " -- did you fill in all the values in StackOverflow (conf, sc, wikiRdd)?")
    import StackOverflow._

    val postings = List(
      Posting(1, 123, None, None, 0, Some("Haskell")),
      Posting(2, 124, None, Some(123), 2, Some("Haskell")),
      Posting(1, 125, None, None, 0, Some("Haskell")),
      Posting(2, 126, None, Some(123), 3, Some("Haskell"))
    )
    val rdd = sc.parallelize(postings)

    val answer = Set(
      (123, List(
        (postings.head, postings.tail.head),
        (postings.head, postings.tail.tail.tail.head)))
    )
    val result = testObject
      .groupedPostings(rdd)
      .collect
      .toSet

    assert(result == answer)
  }

  test("scoredPostings") {
    assert(initializeStackOverflow(), " -- did you fill in all the values in StackOverflow (conf, sc, wikiRdd)?")
    import StackOverflow._

    val postings = List(
      Posting(1, 123, None, None, 0, Some("Haskell")),
      Posting(2, 124, None, Some(123), 2, Some("Haskell")),
      Posting(1, 125, None, None, 0, Some("Haskell")),
      Posting(2, 126, None, Some(123), 3, Some("Haskell")),
      Posting(2, 127, None, Some(126), 23, Some("Haskell"))
    )
    val grouped = List(
      (123, Iterable(
        (postings.head, postings.tail.head),
        (postings.head, postings.tail.tail.tail.head))),
      (126, Iterable(
        (postings.tail.tail.head, postings.tail.tail.tail.tail.head)))
    )
    val rdd = sc.parallelize(grouped)

    val answer = Set(
      (postings.head, 3),
      (postings.tail.tail.head, 23)
    )
    val result = testObject
      .scoredPostings(rdd)
      .collect
      .toSet

    assert(result == answer)
  }

  test("vectorPostings") {
    assert(initializeStackOverflow(), " -- did you fill in all the values in StackOverflow (conf, sc, wikiRdd)?")
    import StackOverflow._

    val postings = List(
      Posting(1, 123, None, None, 0, Some("Haskell")),
      Posting(1, 124, None, None, 0, Some("Haskell")),
      Posting(1, 125, None, None, 0, Some("Ruby"))
    )
    val grouped = List(
      (postings.head, 3),
      (postings.tail.head, 5),
      (postings.tail.tail.head, 23)
    )
    val rdd = sc.parallelize(grouped)

    val answer = Set(
      (550000, 3),
      (550000, 5),
      (300000, 23)
    )
    val result = testObject
      .vectorPostings(rdd)
      .collect
      .toSet

    assert(result == answer)
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }


}
