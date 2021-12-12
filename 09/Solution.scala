import scala.io.Source
type Grid = Vector[Vector[Int]]
type Position = (Int, Int)
type Basin = Set[Position]

def isLowPoint(row: Int, col: Int, grid: Grid): Boolean = {
    val value = grid(row)(col)
    val paddedVectorRow = Vector.fill(grid.head.length)(10)
    val upPosition = grid.applyOrElse(row - 1, (_) => paddedVectorRow).applyOrElse(col, (n) => 10)
    val downPosition = grid.applyOrElse(row + 1, (_) => paddedVectorRow).applyOrElse(col, (n) => 10)
    val leftPosition = grid.applyOrElse(row, (_) => paddedVectorRow).applyOrElse(col - 1, (n) => 10)
    val rightPosition = grid.applyOrElse(row, (_) => paddedVectorRow).applyOrElse(col + 1, (n) => 10)
    List(upPosition, downPosition, leftPosition, rightPosition).forall(value < _)
}

def getLowPoints(grid: Grid): List[Int] = (
    for {
        row <- 0 until grid.length
        col <- 0 until grid.head.length
        if isLowPoint(row, col, grid)
    } yield grid(row)(col)
    ).toList

def getLowPointsPositions(grid: Grid): List[Position] = (
    for {
        row <- 0 until grid.length
        col <- 0 until grid.head.length
        if isLowPoint(row, col, grid)
    } yield (row, col)
    ).toList

def getNeighbours(position: Position, grid: Grid): Set[Position] = {
    val (row, col) = position
    val up = (row - 1, col)
    val down = (row + 1, col)
    val left = (row, col - 1)
    val right = (row, col + 1)
    List(up, down, left, right)
        .filter((x, y) => 0 <= x && x < grid.length && 0 <= y && y < grid.head.length && grid(x)(y) != 9)
        .toSet
}

def growBasin(startPosition: Position, grid: Grid): Basin = {
    def growBasinHelper(visited: Set[Position], currPosition: Position): Set[Position] = {
        val neighbours = getNeighbours(currPosition, grid) -- visited
        if (neighbours.isEmpty) {
            return Set(currPosition)
        } else {
            val updatedVisited = visited | neighbours + currPosition
            neighbours.toList.foldLeft(updatedVisited)((acc, neighbour) => acc ++ growBasinHelper(acc, neighbour))
        }
    }
    growBasinHelper(Set.empty, startPosition)
}

def getBasinSize(basin: Basin): Int = basin.size

def calculateRiskLevel(grid: Grid): Int = getLowPoints(grid).map(_ + 1).sum

def toGrid(rows: List[String]): Grid = rows.map((row) => row.toVector.map(c => (c - '0').toInt)).toVector

def calculate3LargestBasinsProduct(grid: Grid): Int = {
    val lowPointPositions = getLowPointsPositions(grid)
    val basinSizes = lowPointPositions.map((position) => getBasinSize(growBasin(position, grid)) ).sortWith(_ > _)
    basinSizes.take(3).product
}

@main def main = {
    val rows = Source.fromFile("input.txt").getLines.toList
    val grid = toGrid(rows)
    println(calculateRiskLevel(grid))
    val lowPointPositions = getLowPointsPositions(grid)
    println(calculate3LargestBasinsProduct(grid))
}