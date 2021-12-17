import scala.io.Source
type Risk = Int
type Grid = Vector[Vector[Risk]]
type Position = (Int, Int)

def initialiseGrid(grid: Grid) = {
    val positions = Vector.range(1, grid.head.length)
    val initRow = Vector
        .range(1, grid.head.length)
        .foldLeft(Vector.fill(grid.head.length)(0))((acc, i) => acc.updated(i, acc(i - 1) + grid(0)(i)))
    val initCol = Vector
        .range(1, grid.length)
        .foldLeft(Vector.fill(grid.length)(0))((acc, i) => acc.updated(i, acc(i - 1) + grid(i)(0)))
    val newPoints = (for {
        i <- 0 until grid.length
        j <- 0 until grid.head.length
    } yield {
        if (i == 0 && j == 0) {
            (0, 0, 0)
        } else if (i == 0) {
            (0, j, initRow(j))
        } else if (j == 0) {
            (i, 0, initCol(i))
        } else {
            (i, j, 0)
        }
    }).toList
    def reducer(grid: Grid, point: (Int, Int,Int)) = {
        val (x, y, value) = point
        grid.updated(x, grid(x).updated(y, value))
    }
    newPoints.foldLeft(grid)(reducer)
}

def calcLowestRiskDP(position: Position, grid: Grid): Int = {
    val dpGrid = initialiseGrid(grid)
    val updates = (for {
        i <- 1 until grid.length
        j <- 1 until grid.head.length
    } yield(i, j)).toList
    def reducer(acc: Grid, position: Position) = {
        val (x, y) = position
        val increment =  acc(x - 1)(y).min(acc(x)(y - 1))
        acc.updated(x, acc(x).updated(y, grid(x)(y) + increment))
    }
    val finalGrid = updates.foldLeft(dpGrid)(reducer)
    finalGrid(grid.length - 1)(grid.head.length - 1)
}

def calcLowestRisk(grid: Grid): Int = {
    calcLowestRiskDP((0,0), grid)
}

def makeGrid(lines: List[String]): Grid = lines.map(_.map(c => (c - '0').toInt).toVector).toVector

@main def main = {
    val grid = makeGrid(Source.fromFile("input.txt").getLines.toList)
    val t0 = System.nanoTime()
    val lowestRisk = calcLowestRisk(grid)
    val t1 = System.nanoTime()
    println(lowestRisk)
    println(s"Runtime: ${(t1 - t0)/1000000}ms")
}