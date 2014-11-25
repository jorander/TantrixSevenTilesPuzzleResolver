package jorander.tantrixstpr.model

import org.scalatest._
import jorander.tantrixstpr.model.TantrixTile._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TantrixTileSpec extends FlatSpec with Matchers {
  "TantrixTile" should "be constructed with colors for all six edges" in {
    tantrixTile(RED, RED, BLUE, BLUE, GREEN, GREEN)
    .edgeColors.size should be(6)
    
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(Map(TOP -> RED, TOP_RIGHT -> RED))
    }
  }
  
  it should "be constructed with three different colors" in {
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(Map(TOP -> RED, TOP_RIGHT -> RED, 
                    BOTTOM_RIGHT -> RED, BOTTOM -> RED, 
                    BOTTOM_LEFT -> GREEN, TOP_LEFT -> GREEN))
    }
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(Map(TOP -> RED, TOP_RIGHT -> RED, 
                    BOTTOM_RIGHT -> BLUE, BOTTOM -> YELLOW, 
                    BOTTOM_LEFT -> GREEN, TOP_LEFT -> GREEN))
    }
  }
  
  it should "be constructed with two edges of each color" in {
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(Map(TOP -> RED, TOP_RIGHT -> RED, 
                    BOTTOM_RIGHT -> BLUE, BOTTOM -> BLUE, 
                    BOTTOM_LEFT -> BLUE, TOP_LEFT -> GREEN))
    }
  }

  it should "pre-define all 56 tiles" in {
    tile(1) should be (tantrixTile(RED, BLUE, GREEN, RED, BLUE, GREEN))

    a[IllegalArgumentException] should be thrownBy {
      tile(57)
    }
  }
}