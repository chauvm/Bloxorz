package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class FunTestSuite extends FunSuite {

  trait TestSets {
    val levelVector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'));
  }
  
  test("StringParserTerrain") {
    new TestSets {
//      assert(terrainFunction(levelVector)(new Pos(1, 1)), true);
    }
  }
}