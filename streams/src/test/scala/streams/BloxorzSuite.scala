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
      /* terrain for level 1 */

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

  }
  ignore("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(4,4)), "4,4")
      assert(terrain(goal), "4,7")
      assert(terrain(startPos), "1,1")
    }
  }
  ignore("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
      assert(startBlock.isLegal)
      assert(Block(goal, goal).isLegal)
      assert(startBlock.right.isLegal, "right")
      assert(!startBlock.left.isLegal, "left")
      assert(!startBlock.up.isLegal, "up")
      assert(startBlock.down.isLegal, "down")
    }
  }
  ignore("Neighbors functions level 1") {
  	new Level1 {
  		assert(done(Block(goal, goal)))
  		val neighbors = startBlock.neighbors
  		assert(neighbors.length == 4)
  		val legal_neighbors = startBlock.legalNeighbors
  		assert(legal_neighbors.length == 2)
  		val neighbors_history = neighborsWithHistory(startBlock, List())
  		assert(neighbors_history.length == 2)

  	}
  }  
  ignore("optimal solution for level 1") {
    new Level1 {
      println(solution)
      assert(solve(solution) == Block(goal, goal))
    }
  }
  ignore("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  trait Level3 extends SolutionChecker {
  	  	// level 3: 918660
  	val level =
  		"""------ooooooo--
  		  |oooo--ooo--oo--
  		  |ooooooooo--oooo
  		  |oSoo-------ooTo
  		  |oooo--------ooo""".stripMargin
  	val optsolution = List(Right, Left, Up, Down)
  }
  ignore("terrain function level 3") {
  	new Level3 {
  		assert(terrain(startPos), "startPos")
  		assert(terrain(goal), "goal")
  		assert(terrain(Pos(4,0)), "4,0")
  		
  		assert(!terrain(Pos(0,0)), "0,0")
  		assert(!terrain(Pos(0,14)), "0,14")
  		assert(!terrain(Pos(4, 15)), "4,15")
  	}
  }
  
  ignore("findChar level 3") {
  	new Level3 {
      assert(startPos == Pos(3, 1))
  	  assert(goal == Pos(3, 13))
  	  assert(startBlock.right.isLegal, "right")
      assert(!startBlock.left.isLegal, "left")
      assert(startBlock.up.isLegal, "up")
      assert(!startBlock.down.isLegal, "down")
  	}
  }
		 

  ignore("Neighbors functions level 3") {
  	new Level3 {
  		assert(done(Block(goal, goal)))
  		val neighbors = startBlock.neighbors
  		assert(neighbors.length == 4)
  		val legal_neighbors = startBlock.legalNeighbors
  		assert(legal_neighbors.length == 2)
  		val neighbors_history = neighborsWithHistory(startBlock, List())
  		assert(neighbors_history.length == 2)
  		println(neighbors_history)
  	}
  }

  ignore("optimal solution for level 3") {
  	new Level3 {
      println(solution.reverse)
      assert(solve(solution) == Block(goal, goal))
  	}
  }
  
  
  trait Level5 extends SolutionChecker {
  	  	// level 5: 534383
  	val level =
  		"""-----oooooo----
  		  |-----o--ooo----
  		  |-----o--ooooo--
  		  |Sooooo-----oooo
  		  |----ooo----ooTo
  		  |----ooo-----ooo
  		  |------o--oo----
  		  |------ooooo----
  		  |------ooooo----
  		  |-------ooo-----""".stripMargin
  	val optsolution = List(Right, Right, Right, Down, Down, Right, 
  					Down, Down, Right, Down, Right, Up, Left, Left, Left, Up, 
  					Up, Left, Up, Up, Up, Right, Right, Down, Right, Right, Up, 
  					Left, Down, Down, Right, Right, Down, Down, Right)

  }
  test("optimal solution for level 5") {
  	new Level5 {
      println(solution)
      assert(solve(solution) == Block(goal, goal))
  	}
  }

}
