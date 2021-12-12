import scala.io.Source
type Grid = Vector[Vector[Int]]

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

def calculateRiskLevel(grid: Grid): Int = getLowPoints(grid).map(_ + 1).sum

def toGrid(rows: List[String]): Grid = rows.map((row) => row.toVector.map(c => (c - '0').toInt)).toVector

@main def main = {
    val rows = Source.fromFile("input.txt").getLines.toList
    val grid = toGrid(rows)
    println(calculateRiskLevel(grid))
}