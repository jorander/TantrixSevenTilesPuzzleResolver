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

final case class TantrixTile(tileNumber: Int, edgeColors: Map[TileEdge, BandColor]) {
  validation(() => if (hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors)) validationOK else validationError("The tile should have three colors, specified for two edges each.")) match {
    case None => /*Do nothing since assignment is implicit */
    case Some(s) => throw new IllegalArgumentException(s.mkString("|"))
  }

  private def hasThreeColorsSpecifiedForTwoEdgesEach(edgeColors: Map[TileEdge, BandColor]) =
    List(RED, GREEN, BLUE, YELLOW).map(c => edgeColors.values.count(_ == c)).count(_ == 2) == 3

  def edgeColor(edge: TileEdge) = edgeColors getOrElse (edge, throw new IllegalStateException("This should never happen. Missing BandColor for " + edge))

  override def toString = "TantrixTile(tileNumber: " + tileNumber + ")"
}

object TantrixTile {
  def tantrixTile(tileNumber: Int,
    topRightEdgeColor: BandColor,
    rightEdgeColor: BandColor,
    bottomRightEdgeColor: BandColor,
    bottomLeftEdgeColor: BandColor,
    leftEdgeColor: BandColor,
    topLeftEdgeColor: BandColor) =
    TantrixTile(tileNumber, Map(TOP_RIGHT_EDGE -> topRightEdgeColor,
      RIGHT_EDGE -> rightEdgeColor,
      BOTTOM_RIGHT_EDGE -> bottomRightEdgeColor,
      BOTTOM_LEFT_EDGE -> bottomLeftEdgeColor,
      LEFT_EDGE -> leftEdgeColor,
      TOP_LEFT_EDGE -> topLeftEdgeColor))

  private val TILES = Map(
    1 -> tantrixTile(1, BLUE, RED, YELLOW, YELLOW, BLUE, RED),
    7 -> tantrixTile(7, YELLOW, RED, BLUE, BLUE, YELLOW, RED),
    8 -> tantrixTile(8, RED, YELLOW, BLUE, BLUE, RED, YELLOW),
    11 -> tantrixTile(11, RED, BLUE, YELLOW, BLUE, YELLOW, RED),
    13 -> tantrixTile(13, BLUE, YELLOW, RED, RED, YELLOW, BLUE),
    17 -> tantrixTile(17, YELLOW, GREEN, RED, GREEN, RED, YELLOW),
    18 -> tantrixTile(18, YELLOW, RED, GREEN, RED, GREEN, YELLOW),
    22 -> tantrixTile(22, YELLOW, GREEN, RED, RED, GREEN, YELLOW),
    23 -> tantrixTile(23, GREEN, YELLOW, YELLOW, RED, RED, GREEN),
    32 -> tantrixTile(32, YELLOW, GREEN, YELLOW, RED, GREEN, RED),
    34 -> tantrixTile(34, YELLOW, RED, YELLOW, GREEN, RED, GREEN),
    38 -> tantrixTile(38, BLUE, RED, GREEN, RED, GREEN, BLUE),
    55 -> tantrixTile(55, BLUE, GREEN, YELLOW, GREEN, YELLOW, BLUE),
    56 -> tantrixTile(56, BLUE, YELLOW, GREEN, YELLOW, GREEN, BLUE))

  def tile(number: Int) = TILES.getOrElse(number, throw new IllegalArgumentException("Tile number " + number + " does not exist."))
}
