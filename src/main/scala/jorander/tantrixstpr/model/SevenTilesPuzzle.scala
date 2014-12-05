package jorander.tantrixstpr.model

sealed abstract trait TilePosition
case object CENTER_POSITION extends TilePosition
case object LEFT_POSITION extends TilePosition
case object TOP_RIGHT_POSITION extends TilePosition
case object TOP_LEFT_POSITION extends TilePosition
case object RIGHT_POSITION extends TilePosition
case object BOTTOM_RIGHT_POSITION extends TilePosition
case object BOTTOM_LEFT_POSITION extends TilePosition

case class SevenTilesPuzzle(placedTiles: Map[TilePosition, PlacedTantrixTile], unplacedTiles: Set[TantrixTile])

object SevenTilesPuzzle {
  private val tileIntersectionsToCheck: Map[TilePosition, List[(TileEdge, (TilePosition, TileEdge))]] = Map(
    CENTER_POSITION -> List(), // No checks for CenterPosition since it is the first tile
    TOP_RIGHT_POSITION -> List((BOTTOM_LEFT_EDGE, (CENTER_POSITION, TOP_RIGHT_EDGE))),
    RIGHT_POSITION -> List((LEFT_EDGE, (CENTER_POSITION, RIGHT_EDGE)), (TOP_LEFT_EDGE, (TOP_RIGHT_POSITION, BOTTOM_RIGHT_EDGE))),
    BOTTOM_RIGHT_POSITION -> List((TOP_LEFT_EDGE, (CENTER_POSITION, BOTTOM_RIGHT_EDGE)), (TOP_RIGHT_EDGE, (RIGHT_POSITION, BOTTOM_LEFT_EDGE))),
    BOTTOM_LEFT_POSITION -> List((TOP_RIGHT_EDGE, (CENTER_POSITION, BOTTOM_LEFT_EDGE)), (RIGHT_EDGE, (BOTTOM_RIGHT_POSITION, LEFT_EDGE))),
    LEFT_POSITION -> List((RIGHT_EDGE, (CENTER_POSITION, LEFT_EDGE)), (BOTTOM_RIGHT_EDGE, (BOTTOM_LEFT_POSITION, TOP_LEFT_EDGE))),
    TOP_LEFT_POSITION -> List((BOTTOM_RIGHT_EDGE, (CENTER_POSITION, TOP_LEFT_EDGE)), (BOTTOM_LEFT_EDGE, (LEFT_POSITION, TOP_RIGHT_EDGE)), (RIGHT_EDGE, (TOP_RIGHT_POSITION, LEFT_EDGE))))

  def isNewTilePlacementValid(placedTiles: Map[TilePosition, PlacedTantrixTile], position: TilePosition, tile: PlacedTantrixTile): Boolean =
    (tileIntersectionsToCheck.getOrElse(position, throw new IllegalArgumentException("Unknown position: " + position)) match {
      case Nil => List(true)
      case checksToMake => checksToMake.map(_ match {
        case (tileEdgeToCheck, (otherTilePosition, otherTileEdge)) =>
          placedTiles.getOrElse(otherTilePosition,
            throw new IllegalStateException("Tile in position " + otherTilePosition + " must be placed first."))
            .bandColor(otherTileEdge) == tile.bandColor(tileEdgeToCheck)
      })
    }).reduce(_ && _)
}