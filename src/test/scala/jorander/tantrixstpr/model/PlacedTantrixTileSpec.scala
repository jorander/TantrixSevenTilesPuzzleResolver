package jorander.tantrixstpr.model

import TantrixTile._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class PlacedTantrixTileSpec extends FlatSpec with Matchers {
  "PlacedTantrixTile" should "be constructed with a TantrixTile and a number of rotation steps between 0 and 5 (inclusive)" in {
    PlacedTantrixTile(tile(1), 0) should be !== (null)
    a[IllegalArgumentException] should be thrownBy {
      PlacedTantrixTile(null, 0)
    }
    a[IllegalArgumentException] should be thrownBy {
      PlacedTantrixTile(tile(1), -1)
    }
    a[IllegalArgumentException] should be thrownBy {
      PlacedTantrixTile(tile(1), 6)
    }
    try {
      PlacedTantrixTile(null, 7)
      fail("Should have been IllegalArgumentException")
    } catch {
      case iae: IllegalArgumentException => assert(iae.getMessage().contains("|"))
    }
  }

  it should "return the BandColor of the TileEdge at a specified position, with respect to the number of rotation steps" in {
    val tile = tantrixTile(RED, BLUE, YELLOW, RED, BLUE, YELLOW)
    PlacedTantrixTile(tile, 0).bandColor(TOP_EDGE) should be === (RED)
    PlacedTantrixTile(tile, 0).bandColor(TOP_LEFT_EDGE) should be === (YELLOW)
    PlacedTantrixTile(tile, 2).bandColor(TOP_LEFT_EDGE) should be === (RED)
    PlacedTantrixTile(tile, 2).bandColor(TOP_EDGE) should be === (BLUE)
  }
}