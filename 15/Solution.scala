import scala.io.Source
import scala.collection.mutable.PriorityQueue
type Risk = Int
type Grid = Vector[Vector[Risk]]
type Position = (Int, Int)
case class Node (
    position: Position,
    cost: Option[Int]
)

object NodeOrdering extends Ordering[Node] {
  def compare(a:Node, b:Node) =  (a.cost, b.cost) match {
      case (None, None) => 0
      case (Some(x), None) => 1
      case (None, Some(x)) => -1
      case (Some(x), Some(y)) => y compare x
  } 
}

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

def calculateRiskLevel(x: Int, y: Int, grid: Grid): Risk = {
    if (0 <= x && x < grid.length && 0 <= y && y < grid.head.length) {
        grid(x)(y)
    } else {
        // Figure out tile position
        val (tileX, tileY) = (x / grid.length, y / grid.head.length)
        val (seedX, seedY) = (x % grid.length, y % grid.head.length)
        val start = grid(seedX)(seedY) + tileX
        val rowVal = if (start >= 10) start - 9 else start
        val finalVal = rowVal + tileY
        if (finalVal >= 10) finalVal - 9 else finalVal
    }
}

def inRange(value: Int, min: Int, max: Int) = min <= value && value < max

def getNeighbours(position: Position, grid: Grid): Set[Position] = {
    val (x, y) = position
    val up = (x - 1, y)
    val down = (x + 1, y)
    val left = (x, y - 1)
    val right = (x, y + 1)
    Set(up, down, left, right).filter((x1, y1) => inRange(x1, 0, grid.length * 5) && inRange(y1, 0, grid.head.length * 5))
}

def runState(grid: Grid, queue: PriorityQueue[Node], distanceMap: Map[Position, Int]): (Boolean, PriorityQueue[Node], Map[Position, Int]) = {
    val node = queue.dequeue
    val minPos = node.position
    val neighbours = getNeighbours(minPos, grid)
    if (minPos == (grid.length * 5 - 1, grid.head.length * 5 - 1)) {
        (true, queue, distanceMap)
    } else {
        def reducer(acc: Map[Position, Int], position: Position) = {
            val alt = acc.get(minPos).get + calculateRiskLevel(position._1, position._2, grid)
            if (!acc.contains(position) || alt < acc.get(position).get) {
                if (!acc.contains(position)) {
                    queue.enqueue(Node(position, Some(alt)))
                } else {
                    val iterable = queue.map(node => if node.position == position then Node(position, Some(alt)) else node)
                    queue.clear
                    queue.addAll(iterable)
                }
                acc.updated(position, alt)
            } else {
                acc
            }
        }
        val newMap = neighbours.foldLeft(distanceMap)(reducer)
        (false, queue, newMap)
    }

}

def calcLowestRiskDij(grid: Grid): Int = {
    // init distance map
    val initDistanceMap = Map((0,0) -> 0)

    val initQueue = PriorityQueue(Node((0,0), Some(0)))(NodeOrdering)
    
    def getShortestDistance(queue: PriorityQueue[Node], distanceMap: Map[Position, Int]): Int = {
        val (found, nextQueue, nextDist) = runState(grid, queue, distanceMap)
        if (nextQueue.isEmpty) {
            nextDist.get((grid.length * 5 - 1, grid.head.length * 5 - 1)).get
        } else {
            getShortestDistance(nextQueue, nextDist)
        }
    }

    getShortestDistance(initQueue, initDistanceMap)

}

def makeGrid(lines: List[String]): Grid = lines.map(_.map(c => (c - '0').toInt).toVector).toVector

@main def main = {
    val grid = makeGrid(Source.fromFile("input.txt").getLines.toList)
    val t0 = System.nanoTime()
    val lowestRisk = calcLowestRisk(grid)
    val t1 = System.nanoTime()
    println(lowestRisk)
    println(s"Runtime: ${(t1 - t0)/1000000}ms")

    val t2 = System.nanoTime()
    val lowestRisk2 = calcLowestRiskDij(grid)
    val t3 = System.nanoTime()
    println(lowestRisk2)
    println(s"Runtime: ${(t3 - t2)/1000000}ms")

}