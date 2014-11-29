package jorander.tantrixstpr.model

case class PlacedTantrixTile(tile: TantrixTile, nbrOfRotationSteps: Int) {
  validate(
    () => mustNotBeNull(tile, "Tile"),
    () => if (nbrOfRotationSteps >= 0 && nbrOfRotationSteps <= 5)
      validationOK else validationError("Number of Rotation Steps must be between 0 and 5 (inclusive)")) match {
      case None => /*Do nothing since assignment is implicit */
      case Some(s) => throw new IllegalArgumentException(s.mkString("|"))
    }

  private def concatErrors(fs: String*) = fs.toList
  private def validate(validation1: () => Option[String], validation2: () => Option[String]): Option[List[String]] =
    (validation1(), validation2()) match {
      case (None, None) => validationOK
      case (Some(s), None) => validationErrors(concatErrors(s))
      case (None, Some(s)) => validationErrors(concatErrors(s))
      case (Some(s1), Some(s2)) => validationErrors(concatErrors(s1, s2))
    }
  private def validationError(msg: String) = Some(msg)
  private def validationErrors(messages: List[String]) = Some(messages)
  private def validationOK() = None

  private def mustNotBeNull(input: Any, attributeName: String) =
    if (input != null) validationOK else validationError(attributeName + " cannot be null.")

  def bandColor(edgeAsPlaced: TileEdge) = {
    val edgePositions = List(TOP, TOP_RIGHT, BOTTOM_RIGHT, BOTTOM, BOTTOM_LEFT, TOP_LEFT)
    def edgeOfPlacedTile(edgeAsPlaced: TileEdge) = {
      val edgeIndex = edgePositions.indexOf(edgeAsPlaced) - nbrOfRotationSteps
      edgePositions(if (edgeIndex >= 0) edgeIndex else edgeIndex + 6)
    }

    tile.edgeColor(edgeOfPlacedTile(edgeAsPlaced))
  }
}