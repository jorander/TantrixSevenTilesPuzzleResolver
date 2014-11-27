package jorander.tantrixstpr.model

import jorander.scalarop._

case class PlacedTantrixTile(tile: TantrixTile, nbrOfRotationSteps: Int) {
  validateConstructorParams(ConstructorParams(tile, nbrOfRotationSteps)) match {
    case Failure(s) => throw new IllegalArgumentException(s)
    case Success(_) => /*Do nothing */
  }

  private case class ConstructorParams(tile: TantrixTile, nbrOfRotationSteps: Int)
  private def validateConstructorParams(params: ConstructorParams) =
    params ->> validatorsWithConcatenatedErrorstrings(
      (params) => if (params.tile != null) succeed(params) else fail("Tile cannot be null."),
      (params) => if (params.nbrOfRotationSteps >= 0 && params.nbrOfRotationSteps <= 5)
        succeed(params) else fail("Number of Rotation Steps must be between 0 and 5 (inclusive)"))

  private def concatErrors(fs: String*) = fs.toList.mkString("|")
  private def validatorsWithConcatenatedErrorstrings = plus(
      (s1: ConstructorParams, s2: ConstructorParams) => s2, concatErrors)_

  private val edgePositions = List(TOP, TOP_RIGHT, BOTTOM_RIGHT, BOTTOM, BOTTOM_LEFT, TOP_LEFT)
  private def edgeOfPlacedTile(edgeAsPlaced: TileEdge) = {
    val edgeIndex = edgePositions.indexOf(edgeAsPlaced) - nbrOfRotationSteps
    edgePositions(if (edgeIndex >= 0) edgeIndex else edgeIndex + 6)
  }

  def bandColor(edgeAsPlaced: TileEdge) = tile.edgeColor(edgeOfPlacedTile(edgeAsPlaced))
}