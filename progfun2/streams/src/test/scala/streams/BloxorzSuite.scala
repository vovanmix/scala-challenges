package streams

import javafx.geometry.Pos

import org.scalatest.Matchers.{equal, _}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("isStanding") {
    new Level1 {
      val a = Block(Pos(1,1), Pos(1,1))
      assert(a.isStanding)
      val b = Block(Pos(1,1), Pos(1,2))
      assert(!b.isStanding)
    }
  }

  test("isLegal") {
    new Level1 {
      val a = Block(Pos(1,1), Pos(1,1))
      assert(a.isLegal)
      val b = Block(Pos(5,5), Pos(6,5))
      assert(!b.isLegal)
    }
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock == Block(Pos(1,1), Pos(1,1)))
    }
  }

  test("neighbors") {
    new Level1 {
      val a = Block(Pos(1,1), Pos(1,1))
      a.neighbors should equal (List(
        (Block(Pos(1,-1), Pos(1,0)), Left),
        (Block(Pos(1,2), Pos(1,3)), Right),
        (Block(Pos(-1,1), Pos(0,1)), Up),
        (Block(Pos(2,1), Pos(3,1)), Down)
      ))
      val b = Block(Pos(1,1), Pos(1,2))
      b.neighbors should equal (List(
        (Block(Pos(1,0), Pos(1,0)), Left),
        (Block(Pos(1,3), Pos(1,3)), Right),
        (Block(Pos(0,1), Pos(0,2)), Up),
        (Block(Pos(2,1), Pos(2,2)), Down)
      ))
    }
  }

  test("legalNeighbors") {
    new Level1 {
      val a = Block(Pos(0,0), Pos(0,0))
      a.legalNeighbors should equal (List(
        (Block(Pos(0,1), Pos(0,2)), Right),
        (Block(Pos(1,0), Pos(2,0)), Down)
      ))
      val b = Block(Pos(0,0), Pos(0,1))
      b.legalNeighbors should equal (List(
        (Block(Pos(0,2), Pos(0,2)), Right),
        (Block(Pos(1,0), Pos(1,1)), Down)
      ))
    }
  }

  test("done") {
    new Level1 {
      val a = Block(Pos(0,0), Pos(0,0))
      assert(!done(a))
      val b = Block(Pos(4,7), Pos(4,7))
      assert(done(b))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val actual = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).take(2).toSet
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(actual == expected)
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      ) == Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).toStream)
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
