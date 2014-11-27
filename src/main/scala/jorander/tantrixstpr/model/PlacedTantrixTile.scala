package jorander.tantrixstpr.model

case class PlacedTantrixTile(tile: TantrixTile, nbrOfRotationSteps: Int) {
  if (tile == null) throw new IllegalArgumentException("Tile cannot be null.")
  if (nbrOfRotationSteps < 0 || nbrOfRotationSteps > 5) throw new IllegalArgumentException("Number of Rotation Steps must be between 0 and 5 (inclusive)")

  private val edgePositions = List(TOP, TOP_RIGHT, BOTTOM_RIGHT, BOTTOM, BOTTOM_LEFT, TOP_LEFT)
  private def edgeOfPlacedTile(edgeAsPlaced: TileEdge) = {
    val edgeIndex = edgePositions.indexOf(edgeAsPlaced) - nbrOfRotationSteps
    edgePositions(if (edgeIndex >= 0) edgeIndex else edgeIndex + 6)
  }
  
  def bandColor(edgeAsPlaced: TileEdge) = tile.edgeColor(edgeOfPlacedTile(edgeAsPlaced))
}