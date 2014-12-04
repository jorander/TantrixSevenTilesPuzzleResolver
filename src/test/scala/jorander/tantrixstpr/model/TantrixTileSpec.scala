package jorander.tantrixstpr.model

import org.scalatest._
import TantrixTile._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TantrixTileSpec extends FlatSpec with Matchers {
  "TantrixTile" should "be constructed with colors for all six edges" in {
    tantrixTile(99, RED, RED, BLUE, BLUE, GREEN, GREEN)
      .edgeColors.size should be(6)

    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(99, Map(LEFT_EDGE -> RED, TOP_RIGHT_EDGE -> RED))
    }
  }

  it should "be constructed with three different colors" in {
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(99, Map(LEFT_EDGE -> RED, TOP_RIGHT_EDGE -> RED,
        BOTTOM_RIGHT_EDGE -> RED, RIGHT_EDGE -> RED,
        BOTTOM_LEFT_EDGE -> GREEN, TOP_LEFT_EDGE -> GREEN))
    }
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(99, Map(LEFT_EDGE -> RED, TOP_RIGHT_EDGE -> RED,
        BOTTOM_RIGHT_EDGE -> BLUE, RIGHT_EDGE -> YELLOW,
        BOTTOM_LEFT_EDGE -> GREEN, TOP_LEFT_EDGE -> GREEN))
    }
  }

  it should "be constructed with two edges of each color" in {
    a[IllegalArgumentException] should be thrownBy {
      TantrixTile(99, Map(LEFT_EDGE -> RED, TOP_RIGHT_EDGE -> RED,
        BOTTOM_RIGHT_EDGE -> BLUE, RIGHT_EDGE -> BLUE,
        BOTTOM_LEFT_EDGE -> BLUE, TOP_LEFT_EDGE -> GREEN))
    }
  }

  it should "pre-define all 56 tiles" in {
    tile(1) should be(tantrixTile(1, BLUE, RED, YELLOW, YELLOW, BLUE, RED))

    a[IllegalArgumentException] should be thrownBy {
      tile(57)
    }
  }

  it should "return the BandColor for a specified TileEdge" in {
    tile(1) edgeColor BOTTOM_LEFT_EDGE should be === (YELLOW)
  }
}