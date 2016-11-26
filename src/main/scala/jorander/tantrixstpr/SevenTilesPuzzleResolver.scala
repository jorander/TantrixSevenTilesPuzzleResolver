package jorander.tantrixstpr

import jorander.scalarop._
import jorander.tantrixstpr.model._
import TantrixTile._
import SevenTilesPuzzle._

object SevenTilesPuzzleResolver {

  def main(args: Array[String]) {
    args.toStream.map(_.toInt).map((t: Int) => tile(t)) ->> resolvePuzzle(logProgress)_->> printSolutions  

    def logProgress(lastPosition: TilePosition, partialPossibleSolutions: Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]]) = {
      partialPossibleSolutions.foldLeft((0, 0))((res, possibleSolution) => possibleSolution match {
        case Success(_) => (res._1 + 1, res._2)
        case Failure(_) => (res._1, res._2 + 1)
      }) match {
        case (s, f) => Console.out.println("After " + lastPosition + ": " + s + " possible solutions, " + f + " failed solutions.")
      }
      partialPossibleSolutions
    }

    def printSolutions(solutions: Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]]) {
      solutions.foreach(_ match {
        case Success(s) => Console.println("Success: " + s)
        case _ =>
      })
    }
  }
  
  val NO_LOGGING = (lastPosition: TilePosition, partialPossibleSolutions: Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]]) => partialPossibleSolutions

  def resolvePuzzle(logFunc: (TilePosition, Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]]) => Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]])(tiles: Stream[TantrixTile]) = {
    val logFuncPartial = (tilePosition: TilePosition) => logFunc(tilePosition, _: Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]])

    tiles ->>
      startPuzzlesWithTryingAllTilesInCenterPosition ->> logFuncPartial(CENTER_POSITION) ->>
      placeNextTileInPuzzles(TOP_RIGHT_POSITION) ->> logFuncPartial(TOP_RIGHT_POSITION) ->>
      placeNextTileInPuzzles(RIGHT_POSITION) ->> logFuncPartial(RIGHT_POSITION) ->>
      placeNextTileInPuzzles(BOTTOM_RIGHT_POSITION) ->> logFuncPartial(BOTTOM_RIGHT_POSITION) ->>
      placeNextTileInPuzzles(BOTTOM_LEFT_POSITION) ->> logFuncPartial(BOTTOM_LEFT_POSITION) ->>
      placeNextTileInPuzzles(LEFT_POSITION) ->> logFuncPartial(LEFT_POSITION) ->>
      placeNextTileInPuzzles(TOP_LEFT_POSITION) ->> logFuncPartial(TOP_LEFT_POSITION)
  }

  private def placeNextTileInPuzzle(nextPosition: TilePosition)(puzzle: SevenTilesPuzzle) = {
    (for {
      tile <- puzzle.unplacedTiles.toList
      nbrOfRotationSteps <- Range.inclusive(0, 5)
      if (isNewTilePlacementValid(puzzle.placedTiles, nextPosition, PlacedTantrixTile(tile, nbrOfRotationSteps)))
    } yield SevenTilesPuzzle(puzzle.placedTiles + (nextPosition -> PlacedTantrixTile(tile, nbrOfRotationSteps)),
      puzzle.unplacedTiles diff Set(tile))) match {
      case Nil => puzzle ->> fail
      case possibleSolutions => possibleSolutions.toStream ->> succeed
    }
  }

  private def startPuzzlesWithTryingAllTilesInCenterPosition(tiles: Stream[TantrixTile]) =
    tiles.map {
      (tile) => SevenTilesPuzzle(Map(CENTER_POSITION -> PlacedTantrixTile(tile, 0)), tiles.toSet diff Set(tile)) ->> succeed
    }

  private def placeNextTileInPuzzles(nextPosition: TilePosition)(partialPossibleSolutions: Stream[TwoTrackResult[SevenTilesPuzzle, SevenTilesPuzzle]]) =
    partialPossibleSolutions.map(_ ->> bind(placeNextTileInPuzzle(nextPosition)))
      .flatMap(_ match {
        case Success(s) => s map (_ ->> succeed)
        case Failure(f: SevenTilesPuzzle) => List(f ->> fail)
        case default => throw new IllegalArgumentException("Unknown object typ: " + default)
      })
}