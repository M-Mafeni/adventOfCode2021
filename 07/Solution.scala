import scala.io.Source
type CrabPostions = List[Int]
// Map of postion to crab count
type CrabPositionMap = Map[Int, Long]

def calculateCostMap(crabPositionMap: CrabPositionMap, position: Int) = crabPositionMap.map((currPosition, count) => count * (currPosition - position).abs).sum

def toCrabMap(crabPositions: List[Int]): CrabPositionMap = {
    val positionsActive = crabPositions.toSet
    val keys = List.range(positionsActive.min, positionsActive.max + 1)
    keys.lazyZip(keys.map(key => crabPositions.filter(_ == key).length.toLong)).toMap
}

def calculateLeastCost(crabPositionMap: CrabPositionMap): Long = crabPositionMap.keys.map(position => calculateCostMap(crabPositionMap, position)).min 

@main def main = {
    val crabPositions: CrabPostions = Source.fromFile("input.txt").mkString.split(",").map(_.toInt).toList
    println(calculateLeastCost(toCrabMap(crabPositions)))
}