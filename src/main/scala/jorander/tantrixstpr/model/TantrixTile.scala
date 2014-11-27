package jorander.tantrixstpr.model

sealed abstract trait BandColor
case object RED extends BandColor
case object GREEN extends BandColor
case object BLUE extends BandColor
case object YELLOW extends BandColor

sealed abstract trait TileEdge
case object TOP extends TileEdge
case object TOP_LEFT extends TileEdge
case object TOP_RIGHT extends TileEdge
case object BOTTOM extends TileEdge
case object BOTTOM_LEFT extends TileEdge
case object BOTTOM_RIGHT extends TileEdge

final case class TantrixTile(edgeColors: Map[TileEdge, BandColor]) {
  if (!hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors)) throw new IllegalArgumentException("The tile should have three colors, specified for two edges each.")

  private def hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors: Map[TileEdge, BandColor]) =
    List(RED, GREEN, BLUE, YELLOW).map(c => edgeColors.values.count(_ == c)).count(_ == 2) == 3

  def edgeColor(edge: TileEdge) = edgeColors getOrElse (edge, throw new IllegalStateException("This should never happen. Missing BandColor for " + edge))
}

object TantrixTile {
  def tantrixTile(topEdgeColor: BandColor,
    topRightEdgeColor: BandColor,
    bottomRightEdgeColor: BandColor,
    bottomEdgeColor: BandColor,
    bottomLeftEdgeColor: BandColor,
    topLeftEdgeColor: BandColor) =
    TantrixTile(Map(TOP -> topEdgeColor,
      TOP_RIGHT -> topRightEdgeColor,
      BOTTOM_RIGHT -> bottomRightEdgeColor,
      BOTTOM -> bottomEdgeColor,
      BOTTOM_LEFT -> bottomLeftEdgeColor,
      TOP_LEFT -> topLeftEdgeColor))

  private val TILES = Map(1 -> tantrixTile(RED, BLUE, GREEN, RED, BLUE, GREEN))

  def tile(number: Int) = TILES.getOrElse(number, throw new IllegalArgumentException("Tile number " + number + " does not exist."))
}
