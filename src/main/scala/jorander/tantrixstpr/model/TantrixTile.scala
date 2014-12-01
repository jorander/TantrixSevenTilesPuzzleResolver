package jorander.tantrixstpr.model

import jorander.functionalvalidation._

sealed abstract trait BandColor
case object RED extends BandColor
case object GREEN extends BandColor
case object BLUE extends BandColor
case object YELLOW extends BandColor

sealed abstract trait TileEdge
case object TOP_LEFT_EDGE extends TileEdge
case object TOP_RIGHT_EDGE extends TileEdge
case object RIGHT_EDGE extends TileEdge
case object LEFT_EDGE extends TileEdge
case object BOTTOM_LEFT_EDGE extends TileEdge
case object BOTTOM_RIGHT_EDGE extends TileEdge

final case class TantrixTile(edgeColors: Map[TileEdge, BandColor]) {
  validation(() => if (hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors)) validationOK else validationError("The tile should have three colors, specified for two edges each.")) match {
    case None => /*Do nothing since assignment is implicit */
    case Some(s) => throw new IllegalArgumentException(s.mkString("|"))
  }

  private def hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors: Map[TileEdge, BandColor]) =
    List(RED, GREEN, BLUE, YELLOW).map(c => edgeColors.values.count(_ == c)).count(_ == 2) == 3

  def edgeColor(edge: TileEdge) = edgeColors getOrElse (edge, throw new IllegalStateException("This should never happen. Missing BandColor for " + edge))
}

object TantrixTile {
  def tantrixTile(topRightEdgeColor: BandColor,
    rightEdgeColor: BandColor,
    bottomRightEdgeColor: BandColor,
    bottomLeftEdgeColor: BandColor,
    leftEdgeColor: BandColor,
    topLeftEdgeColor: BandColor) =
    TantrixTile(Map(TOP_RIGHT_EDGE -> topRightEdgeColor,
      RIGHT_EDGE -> rightEdgeColor,
      BOTTOM_RIGHT_EDGE -> bottomRightEdgeColor,
      BOTTOM_LEFT_EDGE -> bottomLeftEdgeColor,
      LEFT_EDGE -> leftEdgeColor,
      TOP_LEFT_EDGE -> topLeftEdgeColor))

  private val TILES = Map(1 -> tantrixTile(BLUE, RED, YELLOW, YELLOW, BLUE, RED))

  def tile(number: Int) = TILES.getOrElse(number, throw new IllegalArgumentException("Tile number " + number + " does not exist."))
}
