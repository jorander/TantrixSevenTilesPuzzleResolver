package jorander.tantrixstpr.model

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import jorander.tantrixstpr.model.SevenTilesPuzzle._
import jorander.tantrixstpr.model.TantrixTile._

@RunWith(classOf[JUnitRunner])
class SevenTilesPuzzelSpec extends FlatSpec with Matchers {
  "Function isNewTilePlacementValid" should "always accept the first Tile in the Center position" in {
    isNewTilePlacementValid(Map(), CENTER_POSITION, PlacedTantrixTile(tile(1), 0)) should be(true)
  }

  it should "accept the Tile in the TopRight position if it matches the Tile in the Center position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(1), 0)), TOP_RIGHT_POSITION, PlacedTantrixTile(tile(8), 0)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(1), 1)), TOP_RIGHT_POSITION, PlacedTantrixTile(tile(8), 0)) should be(false)
  }

  it should "not evaluate a Tile in the TopRight position if no Tile has been placed in Center position" in {
    a[IllegalStateException] should be thrownBy {
      isNewTilePlacementValid(Map(), TOP_RIGHT_POSITION, PlacedTantrixTile(tile(1), 0))
    }
  }

  it should "accept the Tile in the Right position if it matches the Tile in the Center position and the Tile in the TopRight position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 1), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 1)), RIGHT_POSITION, PlacedTantrixTile(tile(8), 5)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 1), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 1)), RIGHT_POSITION, PlacedTantrixTile(tile(8), 3)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 1), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 1)), RIGHT_POSITION, PlacedTantrixTile(tile(8), 1)) should be(false)
  }

  it should "not evaluate a Tile in the Right position if no Tile has been placed in Center position" in {
    a[IllegalStateException] should be thrownBy {
      isNewTilePlacementValid(Map(TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 1)), RIGHT_POSITION, PlacedTantrixTile(tile(8), 5))
    }
  }

  it should "not evaluate a Tile in the Right position if no Tile has been placed in TopRight position" in {
    a[IllegalStateException] should be thrownBy {
      isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 1)), RIGHT_POSITION, PlacedTantrixTile(tile(8), 5))
    }
  }

  it should "accept the Tile in the BottomRight position if it matches the Tile in the Center position and the Tile in the Right position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 2), RIGHT_POSITION -> PlacedTantrixTile(tile(1), 2)), BOTTOM_RIGHT_POSITION, PlacedTantrixTile(tile(8), 0)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 2), RIGHT_POSITION -> PlacedTantrixTile(tile(1), 2)), BOTTOM_RIGHT_POSITION, PlacedTantrixTile(tile(8), 4)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 2), RIGHT_POSITION -> PlacedTantrixTile(tile(1), 2)), BOTTOM_RIGHT_POSITION, PlacedTantrixTile(tile(8), 2)) should be(false)
  }
  
  it should "accept the Tile in the BottomLeft position if it matches the Tile in the Center position and the Tile in the BottomRight position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 3), BOTTOM_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 3)), BOTTOM_LEFT_POSITION, PlacedTantrixTile(tile(8), 1)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 3), BOTTOM_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 3)), BOTTOM_LEFT_POSITION, PlacedTantrixTile(tile(8), 5)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 3), BOTTOM_RIGHT_POSITION -> PlacedTantrixTile(tile(1), 3)), BOTTOM_LEFT_POSITION, PlacedTantrixTile(tile(8), 3)) should be(false)
  }
  
  it should "accept the Tile in the Left position if it matches the Tile in the Center position and the Tile in the BottomLeft position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 4), BOTTOM_LEFT_POSITION -> PlacedTantrixTile(tile(1), 4)), LEFT_POSITION, PlacedTantrixTile(tile(8), 2)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 4), BOTTOM_LEFT_POSITION -> PlacedTantrixTile(tile(1), 4)), LEFT_POSITION, PlacedTantrixTile(tile(8), 0)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 4), BOTTOM_LEFT_POSITION -> PlacedTantrixTile(tile(1), 4)), LEFT_POSITION, PlacedTantrixTile(tile(8), 4)) should be(false)
  }
  
  it should "accept the Tile in the TopLeft position if it matches the Tile in the Center position, the Tile in the Left position and the Tile in the TopRight position" in {
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 5), LEFT_POSITION -> PlacedTantrixTile(tile(1), 5), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(22),2)), TOP_LEFT_POSITION, PlacedTantrixTile(tile(8), 3)) should be(true)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 5), LEFT_POSITION -> PlacedTantrixTile(tile(1), 5), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(22),2)), TOP_LEFT_POSITION, PlacedTantrixTile(tile(8), 1)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 5), LEFT_POSITION -> PlacedTantrixTile(tile(1), 5), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(56),5)), TOP_LEFT_POSITION, PlacedTantrixTile(tile(8), 5)) should be(false)
    isNewTilePlacementValid(Map(CENTER_POSITION -> PlacedTantrixTile(tile(17), 5), LEFT_POSITION -> PlacedTantrixTile(tile(1), 5), TOP_RIGHT_POSITION -> PlacedTantrixTile(tile(22),5)), TOP_LEFT_POSITION, PlacedTantrixTile(tile(8), 3)) should be(false)
  }
}