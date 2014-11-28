package jorander.tantrixstpr.model

import jorander.scalarop._

case class PlacedTantrixTile(tile: TantrixTile, nbrOfRotationSteps: Int) {
  (tile, nbrOfRotationSteps) ->> validate(
    mustNotBeNull[(TantrixTile, Int)](_._1, "Tile"),
    (params: (TantrixTile, Int)) => if (params._2 >= 0 && params._2 <= 5)
      succeed(params) else fail("Number of Rotation Steps must be between 0 and 5 (inclusive)")) match {
      case Failure(s) => throw new IllegalArgumentException(s.mkString("|"))
      case Success(_) => /*Do nothing since assignment is implicit */
    }

  private def concatErrors(fs: String*) = fs.toList
  private def validate[I] = plus((i1: I, i2: I) => i2, concatErrors)_
  private def mustNotBeNull[I](dataAccessor: I => Any, attributeName: String)(input: I) =
    if (dataAccessor(input) != null) succeed(input) else fail(attributeName + " cannot be null.")

  def bandColor(edgeAsPlaced: TileEdge) = {
    val edgePositions = List(TOP, TOP_RIGHT, BOTTOM_RIGHT, BOTTOM, BOTTOM_LEFT, TOP_LEFT)
    def edgeOfPlacedTile(edgeAsPlaced: TileEdge) = {
      val edgeIndex = edgePositions.indexOf(edgeAsPlaced) - nbrOfRotationSteps
      edgePositions(if (edgeIndex >= 0) edgeIndex else edgeIndex + 6)
    }

    tile.edgeColor(edgeOfPlacedTile(edgeAsPlaced))
  }
}