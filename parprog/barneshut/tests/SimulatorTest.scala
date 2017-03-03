package barneshut

import org.scalatest.FunSuite

/**
  * Created by vlad on 3/2/17.
  */
class SimulatorTest extends FunSuite {

  test("testMergeBoundaries") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 33
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 33

    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val boundaries2 = simulator.updateBoundaries(boundaries, body)
    assert(boundaries2.minX == 25)
    assert(boundaries2.maxY == 47)
  }

//    [Test Description] 'mergeBoundaries' should correctly merge two boundaries [Observed Error] 45.0 equaled 45, but 89.0 did not equal 117 minX and maxX are not correctly updated [Lost Points] 2

  test("testUpdateBoundaries") {
    val boundaries1 = new Boundaries()
    boundaries1.minX = 22
    boundaries1.minY = 22
    boundaries1.maxX = 33
    boundaries1.maxY = 33

    val boundaries2 = new Boundaries()
    boundaries2.minX = 1
    boundaries2.minY = 1
    boundaries2.maxX = 55
    boundaries2.maxY = 55

    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val boundaries3 = simulator.mergeBoundaries(boundaries1, boundaries2)
    assert(boundaries3.minX == 1)
    assert(boundaries3.minY == 1)
    assert(boundaries3.maxX == 33.0)
    assert(boundaries3.maxY == 33.0)
  }

  test("computeSectorMatrix") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97

    val bodies = Seq(
      new Body(5, 25, 47, 0.1f, 0.1f),
      new Body(5, 26, 47, 0.1f, 0.1f),
      new Body(5, 27, 47, 0.1f, 0.1f),
      new Body(6, 26, 48, 0.1f, 0.1f),
      new Body(7, 27, 49, 0.1f, 0.1f),
      new Body(8, 28, 50, 0.1f, 0.1f),
      new Body(9, 29, 51, 0.1f, 0.1f),
      new Body(10, 30, 52, 0.1f, 0.1f)
    )

    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val sm = simulator.computeSectorMatrix(bodies, boundaries)

    assert(sm(2, 3).size == 3)
    assert(sm(2, 3).exists(_ == bodies.head))
  }

//    [Test Description] 'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector [Observed Error] ConcBuffer() had size 0 instead of expected size 1 bucket (0,2) should have size 1 [Lost Points] 2

  test("updateBodies"){
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97

    val bodies = Seq(
      new Body(5, 25, 47, 0.1f, 12f),
      new Body(6, 26, 48, 0.1f, 14f),
      new Body(7, 27, 49, 0.1f, 1f)
    )
    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val sm = simulator.computeSectorMatrix(bodies, boundaries)
    val res = simulator.updateBodies(bodies, sm.toQuad(1))

    assert(
      res.map(b => s"${b.x} ${b.y}") ==
      Seq("25.001 47.12", "26.001 48.14", "27.001 49.01")
    )
  }
}
