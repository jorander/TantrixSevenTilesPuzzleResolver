package jorander.tantrixstpr

import jorander.scalarop._
import jorander.tantrixstpr.model.TantrixTile
import SevenTilesPuzzleResolver._

case class SevenTilesPuzzleAnalysis(tiles: Set[TantrixTile], nbrOfSolutions: Int) {
}

object SevenTilesPuzzleAnalysis {

  def main(args: Array[String]) {

    def analyzePuzzleSolutions(tiles: Set[TantrixTile]) = SevenTilesPuzzleAnalysis(tiles,
      (tiles ->> resolvePuzzle(NO_LOGGING)).foldLeft(0)((nbrOfSolutions, possibleSolution) => possibleSolution match {
        case Success(_) => nbrOfSolutions + 1
        case _ => nbrOfSolutions
      }))

    TantrixTile.allTiles.subsets(7)
      .toStream.par.map(analyzePuzzleSolutions)
      .groupBy(_.nbrOfSolutions)
      .mapValues(_.size)
      .toList.sortBy(_._1)
      .foreach(_ match {
        case (nbrOfSolutions, nbrOfCombinations) => Console.println(nbrOfSolutions + ": nbrCombinations: " + nbrOfCombinations)
      })
  }
}