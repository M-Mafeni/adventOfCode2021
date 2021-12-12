import scala.io.Source
type Octopus = Int
type Grid = Vector[Vector[Octopus]]

val example = Vector(Vector(1, 1, 1, 1, 1), Vector(1, 9, 9, 9, 1), Vector(1, 9, 1, 9, 1), Vector(1, 9, 9, 9, 1), Vector(1, 1, 1, 1, 1))

def hasEnergyLevelGreaterThanNine(grid: Grid): Boolean = grid.exists(row => row.exists(_ > 9))
def inRange(x: Int, minVal: Int, maxVal: Int): Boolean = minVal <= x && x < maxVal
def getNeighbours(row: Int, col: Int, grid: Grid): List[Octopus] = (for {
        i <- (row - 1) until (row + 2)
        j <- (col - 1) until (col + 2)
        if (inRange(i, 0, grid.length) && inRange(j, 0, grid.head.length) && (i,j) != (row, col))
    } yield grid(i)(j)).toList

def runFlashes(grid: Grid): Grid = {
    def updateLevel(row: Int, col: Int) = {
        val level = grid(row)(col)
        val neighbours = getNeighbours(row, col, grid)
        val increment = neighbours.count(_ > 9)
        if (level == 0 || level > 9) {
            0
        } else {
            level + increment
        }
    }
    if (!hasEnergyLevelGreaterThanNine(grid)) {
        grid
    } else {
        val rowIndexes = List.range(0, grid.length).toVector
        val newGrid = rowIndexes.map(row => List.range(0, grid.head.length).toVector.map(col => updateLevel(row, col)))
        runFlashes(newGrid)
    }
}
// Given the grid return the no of flashes that occured + a new grid
def runStep(grid: Grid): (Int, Grid) = {
    // increase every energy level by one
    val increasedByOne = grid.map(row => row.map(oct => oct + 1))
    val afterFlashes = runFlashes(increasedByOne)
    val flashNo = grid.map((row) => row.count(_ == 0)).sum
    (flashNo, afterFlashes)
}

def allFlash(grid: Grid): Boolean = grid.forall(row => row.forall(_ == 0))
def getStepAllFlashHelper(grid: Grid, steps: Int): Int = {
    val (_, nextGrid) = runStep(grid)
    if allFlash(nextGrid) then steps + 1 else getStepAllFlashHelper(nextGrid, steps + 1)
}

// Given the grid return the no of flashes that occured + a new grid
def getStepAllFlash(grid: Grid): Int = getStepAllFlashHelper(grid, 0)

def runStepsHelper(steps: Int, grid: Grid, flashTotal: Int): Int = {
    val (flashes, nextGrid) = runStep(grid)
    if (steps == 0) {
        flashTotal + flashes
    } else {
        runStepsHelper(steps - 1, nextGrid, flashTotal + flashes)
    }
}
// run steps and return the total no of flashes
def runSteps(steps: Int, grid: Grid): Int = runStepsHelper(steps, grid, 0)

def toGrid(lines: List[String]): Grid = lines.map(line => line.toVector.map(x => (x - '0').toInt)).toVector

def prettyGrid(grid: Grid): String = "\n" + grid.map(row => row.map(value => ('0' + value).toChar).mkString).mkString("\n")

@main def main = {
    val grid = toGrid(Source.fromFile("input.txt").getLines.toList)
    println(runSteps(100, grid))
    println(getStepAllFlash(grid))
}