package jorander.tantrixstpr.model

import jorander.functionalvalidation._

case class PlacedTantrixTile(tile: TantrixTile, nbrOfRotationSteps: Int) {
  validation(
    () => mustNotBeNull(tile, "Tile"),
    () => if (nbrOfRotationSteps >= 0 && nbrOfRotationSteps <= 5)
      validationOK else validationError("Number of Rotation Steps must be between 0 and 5 (inclusive)")) match {
      case None => /*Do nothing since assignment is implicit */
      case Some(s) => throw new IllegalArgumentException(s.mkString("|"))
    }

  def bandColor(edgeAsPlaced: TileEdge) = {
    val edgePositions = List(TOP_EDGE, TOP_RIGHT_EDGE, BOTTOM_RIGHT_EDGE, BOTTOM_EDGE, BOTTOM_LEFT_EDGE, TOP_LEFT_EDGE)
    def edgeOfPlacedTile(edgeAsPlaced: TileEdge) = {
      val edgeIndex = edgePositions.indexOf(edgeAsPlaced) - nbrOfRotationSteps
      edgePositions(if (edgeIndex >= 0) edgeIndex else edgeIndex + 6)
    }

    tile.edgeColor(edgeOfPlacedTile(edgeAsPlaced))
  }
}