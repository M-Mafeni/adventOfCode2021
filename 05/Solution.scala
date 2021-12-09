import scala.io.Source
type Point = (Int, Int)
case class Line(
    start: Point,
    end: Point
)

def inHorizontalRange(line: Line, point: Point): Boolean = (line.start._1 == point._1) && line.start._2 <= point._2 && point._2 <= line.end._2
def inVerticalRange(line: Line, point: Point): Boolean =  (line.start._2 == point._2) && line.start._1 <= point._1 && point._1 <= line.end._1

def isHorizontal(line: Line): Boolean = line.start._1 == line.end._1
def isVertical(line: Line): Boolean = line.start._2 == line.end._2
def isDiagonal(line: Line): Boolean = !(isHorizontal(line) || isVertical(line))

def genHorizontalPoints(line1: Line, line2: Line, startPoint: Point): Set[Point] = {
    val points = for (
        y <- startPoint._2 until line2.end._2.min(line1.end._2) + 1
    ) yield (startPoint._1, y)
    return points.toSet
}

def genVerticalPoints(line1: Line, line2: Line, startPoint: Point): Set[Point] = {
    val points = for (
        x <- startPoint._1 until line1.end._1.min(line2.end._1) + 1
    ) yield (x, startPoint._2)
    return points.toSet
}

def genHorizontalAndVerticalPoints(horizontal: Line, vertical: Line): Set[Point] = {

     // See if there's a generic overlapping point
    val intersectionPoint = (horizontal.start._1, vertical.start._2)

    if (inHorizontalRange(horizontal, intersectionPoint) && inVerticalRange(vertical, intersectionPoint)) {
        return Set(intersectionPoint)
    }
    return Set()
}

def runOverlappingTest(line1: Line, line2: Line, expected: Set[Point]) = {
    val points = getOverlappingPoints(line1, line2)
    assert(points.equals(expected))
}

def testOverlappingPoints() = {
    println("Begin Horizontal Tests")
    runOverlappingTest(Line((0,0),(0,3)), Line((0,4),(0,6)), Set())
    runOverlappingTest(Line((0,0),(0,3)), Line((0,3),(0,5)), Set((0,3)))
    runOverlappingTest(Line((0,0),(0,3)), Line((0,1),(0,3)), Set((0,1), (0,2), (0,3)))
    runOverlappingTest(Line((0,1),(0,3)), Line((0,0),(0,3)), Set((0,1), (0,2), (0,3)))
    runOverlappingTest(Line((0,0),(0,3)), Line((0,1),(0,5)), Set((0,1), (0,2), (0,3)))
    runOverlappingTest(Line((0,0),(0,3)), Line((0,0),(0,3)), Set((0,0), (0,1), (0,2), (0,3)))
    runOverlappingTest(Line((0,0),(0,3)), Line((0,3),(0,5)), Set((0,3)))
    println("Horizontal Tests Complete")
    println("Begin Vertical Tests")
    runOverlappingTest(Line((0,0),(3,0)), Line((4,0),(6,0)), Set())
    runOverlappingTest(Line((0,0),(3,0)), Line((3,0),(5,0)), Set((3,0)))
    runOverlappingTest(Line((0,0),(3,0)), Line((1,0),(3,0)), Set((1,0), (2,0), (3,0)))
    runOverlappingTest(Line((1,0),(3,0)), Line((0,0),(3,0)), Set((1,0), (2,0), (3,0)))
    runOverlappingTest(Line((0,0),(3,0)), Line((1,0),(5,0)), Set((1,0), (2,0), (3,0)))
    runOverlappingTest(Line((0,0),(3,0)), Line((0,0),(3,0)), Set((0,0), (1,0), (2,0), (3,0)))
    runOverlappingTest(Line((0,0),(3,0)), Line((3,0),(5,0)), Set((3,0)))
    println("Vertical Tests Complete")
    println("Begin Horizontal + Vertical Tests")
    runOverlappingTest(Line((0,0),(3,0)), Line((1,1),(1,3)), Set())
    runOverlappingTest(Line((0,0),(3,0)), Line((1,1),(1,3)), Set())
    println("Horizontal + Vertical Tests Complete")
}

def getOverlappingPoints(line1: Line, line2: Line): Set[Point] = {
    (isHorizontal(line1), isHorizontal(line2)) match {
        case (true, true) => {
            if (inHorizontalRange(line1, line2.start)) {
                return genHorizontalPoints(line1, line2, line2.start)
            } else if (inHorizontalRange(line2, line1.start)) {
                return genHorizontalPoints(line1, line2, line1.start)
            } else {
                return Set()
            }
        }
        case (false, false) => {
            // Vertical
            if (inVerticalRange(line1, line2.start)) {
                return genVerticalPoints(line1, line2, line2.start)
            } else if (inVerticalRange(line2, line1.start)) {
                return genVerticalPoints(line1, line2, line1.start)
            } else {
                return Set()
            }
        }
        case (true, false) => {
            return genHorizontalAndVerticalPoints(line1, line2)
        }
        case (false, true) => {
            return genHorizontalAndVerticalPoints(line2, line1)
        }
    }
}

// Assume all lines are horizontal and vertical
def countOverlappingPointsPart1(lines: List[Line]): Int = {
    val linePairs = for {
        l1 <- lines
        l2 <- lines if l1 != l2
    } yield (l1, l2)
    val overlappingPoints = linePairs.map(getOverlappingPoints)
    if (overlappingPoints.isEmpty) {
        return 0
    }
    val finPoints = overlappingPoints.reduceLeft((acc, s1) => acc.union(s1))
    return finPoints.size
}

def getPoint(text: String): Point = {
    val nums = text.split(",").map(_.toInt).toList
    return (nums.head, nums.tail.head)
}

def getLine(text: String): Line = {
    val points = text.split(" -> ").map(getPoint).sorted
    return Line(points.head, points.tail.head)
}
def getLinesFromFile(filename: String): List[Line] = {
    val lines = Source.fromFile(filename).getLines.map(getLine).toList
    return lines
}

@main def main = {
    val filename = "input.txt"
    val lines = getLinesFromFile(filename)
    // Filter out diagonal lines
    println(countOverlappingPointsPart1(lines.filterNot(isDiagonal)))
}