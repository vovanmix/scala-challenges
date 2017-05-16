package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{
DoubleType,
StringType,
StructField,
StructType
}
import timeusage.TimeUsage._

import org.apache.spark.sql._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}


@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
  var ss: SparkSession = _

  override def beforeAll(): Unit = {
    ss = SparkSession.builder()
      .appName("unit-test")
      .config("spark.master", "local")
      .getOrCreate()

    // reduce the number of DF partitions
    ss.conf.set("spark.sql.shuffle.partitions", 10)
  }

  override def afterAll(): Unit = {
    ss.close()
    ss = null
    spark.stop()
  }

  test("schema") {
    val res = StructType(
      StructField("ca", StringType, false)
        :: StructField("cb", DoubleType, false)
        :: StructField("cc", DoubleType, false)
        :: Nil
    )
    assert(TimeUsage.dfSchema("ca" :: "cb" :: "cc" :: Nil) === res)
  }

  test("row should work correctly") {
    val res = Row fromSeq List("foo", 1.0, 2.0)
    val input = List("foo", "1.0", "2.0")
    assert(TimeUsage.row(input) === res)
  }

  test("classifiedColumns should work correctly for one column") {
    val columnNames = List("t18010")
    val (g1, g2, g3) = TimeUsage.classifiedColumns(columnNames)

    assert(g1 === List(new Column("t18010")))
    assert(g2 === List.empty)
    assert(g3 === List.empty)
  }

  test("'classifiedColumns' should work as expected") {
    val (columns, _) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsCols, workingCols, otherCols) =
      TimeUsage.classifiedColumns(columns)

    assert(primaryNeedsCols.size == 55, "no. of private needs cols")
    assert(workingCols.size == 23, "no. of working activities cols")
    assert(otherCols.size == 346, "no. of other activities cols")
  }

  test("'timeUsageSummary' should work as expected") {
    val (colNames, df) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsCols, workingCols, otherCols) = TimeUsage.classifiedColumns(colNames)

    val tuDF = TimeUsage.timeUsageSummary(primaryNeedsCols, workingCols, otherCols, df)

    tuDF.show()

    val cols = workingCols ++ List(new Column("telfs"))

    df.select(cols: _*).show()
  }

  test("'timeUsageGrouped' should work as expected") {
    val (colNames, df) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsCols, workingCols, otherCols) = TimeUsage.classifiedColumns(colNames)

    val summed = TimeUsage.timeUsageSummary(primaryNeedsCols, workingCols, otherCols, df)
    //    summed.show()

    val grouped = TimeUsage.timeUsageGrouped(summed)
    grouped.show()
  }

  test("'timeUsageGroupedSql' should work as expected") {
    val (colNames, df) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsCols, workingCols, otherCols) = TimeUsage.classifiedColumns(colNames)

    val summed = TimeUsage.timeUsageSummary(primaryNeedsCols, workingCols, otherCols, df)
    //    summed.show()

    val grouped = TimeUsage.timeUsageGroupedSql(summed)
    grouped.show()
  }

  test("'timeUsageGroupedTyped' should work as expected") {
    val (colNames, df) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsCols, workingCols, otherCols) = TimeUsage.classifiedColumns(colNames)

    val summed = TimeUsage.timeUsageSummary(primaryNeedsCols, workingCols, otherCols, df)
    val summedGrouped = TimeUsage.timeUsageSummaryTyped(summed)

    val grouped = TimeUsage.timeUsageGroupedTyped(summedGrouped)
    grouped.show()
  }

  test("TimeUsage can be instantiated") {
    val instantiatable = try {
      TimeUsage
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a TimeUsage object")
  }

  test("row") {
    val r = TimeUsage.row(List("string", "1", "2"))
    assert(r(0) === "string")
    assert(r(1) === 1)
    assert(r(2) === 2)
  }

  test("timeUsageSummary") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

    val u = summaryDf.first()

    assert(u(0) === "working")
    assert(u(1) === "male")
    assert(u(2) === "elder")
    assert(u(3) === 15.25)
    assert(u(4) === 0.0)
    assert(u(5) === 8.75)
  }

  test("timeUsageGrouped") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGrouped(summaryDf)


    val u = finalDf.first()

    assert(u(0) === "not working")
    assert(u(1) === "female")
    assert(u(2) === "active")
    assert(u(3) === 12.4)
    assert(u(4) === 0.5)
    assert(u(5) === 10.8)
  }

  test("timeUsageGroupedSql") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGroupedSql(summaryDf)


    val u = finalDf.first()

    assert(u(0) === "not working")
    assert(u(1) === "female")
    assert(u(2) === "active")
    assert(u(3) === 12.4)
    assert(u(4) === 0.5)
    assert(u(5) === 10.8)
  }

  test("timeUsageSummaryTyped") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val summaryDs = timeUsageSummaryTyped(summaryDf)

    summaryDs.show()
    val u = summaryDs.first()

    assert(u.working === "working")
    assert(u.sex === "male")
    assert(u.age === "elder")
    assert(u.primaryNeeds === 15.25)
    assert(u.work === 0.0)
    assert(u.other === 8.75)
  }

  test("timeUsageGroupedTyped") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val summaryDs = timeUsageSummaryTyped(summaryDf)
    val finalDf = timeUsageGroupedTyped(summaryDs)

    val u = finalDf.first()

    assert(u.working === "not working")
    assert(u.sex === "female")
    assert(u.age === "active")
    assert(u.primaryNeeds === 12.4)
    assert(u.work === 0.5)
    assert(u.other === 10.8)
  }
}
