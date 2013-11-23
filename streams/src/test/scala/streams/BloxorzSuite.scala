package streams

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
      ls.foldLeft(startBlock) { case (block, move) => move match {
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

  test("TerrainFunctionParse") {
    new Level1 {
      val v = Vector(Vector('-','S', 'T'), Vector('-','o', 'o'), Vector('o', 'o','-'))
      val f = terrainFunction(v)
      assert(f(Pos(-1,0)) === false)
      assert(f(Pos(0,-1)) === false)
      assert(f(Pos(0,10)) === false)
      assert(f(Pos(10,0)) === false)
      assert(f(Pos(10,0)) === false)
      assert(f(Pos(0,0)) === false)
      assert(f(Pos(1,1)) === true)
    }
  }

  test("Neighbours with history") {
    new Level1 {
      val r = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(r.toSet === expected)
    }
  }

  test("New neighbours only") {
    new Level1 {
      val r = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      val expected =  Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(r.toSet === expected)
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("done function") {
    new Level1 {
      assert(done(Block(Pos(4,7), Pos(4,7))), "done ((4,7), (4,7))")
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      println ("solution= " + solution)
      assert(solution.length == optsolution.length)
    }
  }
}
