import scala.io.Source
import math.Ordered.orderingToOrdered

type Point = (Int, Int)
case class Line(
    start: Point,
    end: Point
)

case class LineEquation (
    gradient: Int,
    constant: Int,
)

def getLineEquation(line: Line): LineEquation = {
    val gradient = (line.end._2 - line.start._2) / (line.end._1 - line.start._1)
    val constant = line.start._2 - gradient * line.start._1
    return LineEquation(gradient, constant)
}

def pointOnLineEquation(lineEquation: LineEquation, point: Point): Boolean = {
    val y = lineEquation.gradient * point._1 + lineEquation.constant
    return y == point._2
}

def getYCoord(lineEquation: LineEquation, x: Int): Int = {
    return (lineEquation.gradient * x + lineEquation.constant)
}

def getXCoord(lineEquation: LineEquation, y: Int): Int = {
    return (y - lineEquation.constant) / lineEquation.gradient
}

def inHorizontalRange(line: Line, point: Point): Boolean = (line.start._1 == point._1) && line.start._2 <= point._2 && point._2 <= line.end._2
def inVerticalRange(line: Line, point: Point): Boolean =  (line.start._2 == point._2) && line.start._1 <= point._1 && point._1 <= line.end._1
def inDiagonalRange(line: Line, point: Point): Boolean = {
    val lineEquation = getLineEquation(line)
    return pointOnLineEquation(lineEquation, point) && line.start <= point && point <= line.end
}

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

def genDiagonalPoints(lineEquation: LineEquation, line1: Line, line2: Line, startPoint: Point): Set[Point] = {
    // val points = for (
    //     x <- startPoint._1 until line1.end._1.min(line2.end._1) + 1;
    //     y <- startPoint._2 until line2.end._2.min(line1.end._2) + 1
    // ) yield (x, y)
    val xCoords = List.range(startPoint._1,  line1.end._1.min(line2.end._1) + 1)
    val coords = xCoords.map((x) => (x,getYCoord(lineEquation, x)))
    return coords.toSet
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
    println("Horizontal + Vertical Tests Complete")
    println("Begin Horizontal + Diagonal Tests")
    runOverlappingTest(Line((0,0), (5,5)), Line((1,1), (1,5)), Set((1,1)))
    runOverlappingTest(Line((-5,-5), (0,0)), Line((1,1), (1,5)), Set())
    println("Horizontal + Diagonal Tests Complete")
    println("Begin Diagonal + Diagonal Tests")
    runOverlappingTest(Line((0,0), (5,5)), Line((1,1), (2,2)), Set((1,1),(2,2)))
    runOverlappingTest(Line((0,0), (5,-5)), Line((1,-1), (2,-2)), Set((1,-1),(2,-2)))
    println("Diagonal + Diagonal Tests Complete")
}

def getOverlappingPointDiagonalAndNonDiagonal(diagonal: Line, nonDiagonal: Line): Set[Point] = {
    val lineEquation = getLineEquation(diagonal)

    if (isHorizontal(nonDiagonal)) {
        val intersectionPoint = (nonDiagonal.start._1, getYCoord(lineEquation, nonDiagonal.start._1))
        return if (inHorizontalRange(nonDiagonal, intersectionPoint) && inDiagonalRange(diagonal, intersectionPoint)) Set(intersectionPoint) else Set()
    } else {
        // Vertical
        val intersectionPoint = (getXCoord(lineEquation, nonDiagonal.start._2), nonDiagonal.start._2)
        return if (inVerticalRange(nonDiagonal, intersectionPoint) && inDiagonalRange(diagonal, intersectionPoint)) Set(intersectionPoint) else Set()
    }
}

def calcDiagonalsIntersectionXCoord(lineEquation1: LineEquation, lineEquation2: LineEquation): Int = {
    if ((lineEquation2.gradient - lineEquation1.gradient) == 0) {
        return 0
    }
    return (lineEquation1.constant - lineEquation2.constant) / (lineEquation2.gradient - lineEquation1.gradient)
}

def getOverlappingPoints(line1: Line, line2: Line): Set[Point] = {
    (isDiagonal(line1), isDiagonal(line2)) match {
        case(false, false) => getOverlappingPointsNoDiagonal(line1, line2)
        case(true, false) => getOverlappingPointDiagonalAndNonDiagonal(line1, line2)
        case (false, true) => getOverlappingPointDiagonalAndNonDiagonal(line2, line1)
        case (true, true) => {
            val lineEquation1 = getLineEquation(line1)
            val lineEquation2 = getLineEquation(line2)
            val direction1 = lineEquation1.gradient > 0
            val direction2 = lineEquation2.gradient > 0;
            if (direction1 != direction2) {
                val xIntersect = calcDiagonalsIntersectionXCoord(lineEquation1, lineEquation2)
                val y1 = getYCoord(lineEquation1, xIntersect)
                val y2 = getYCoord(lineEquation2, xIntersect)
                val intersectionPoint = (xIntersect, y1)
                return if (y1 == y2 && inDiagonalRange(line1, intersectionPoint) && inDiagonalRange(line2, intersectionPoint)) Set((xIntersect, y1)) else Set()
            } else {
                if (inDiagonalRange(line1, line2.start)) {
                    return genDiagonalPoints(lineEquation1, line1, line2, line2.start)
                } else if (inDiagonalRange(line2, line1.start)) {
                    return genDiagonalPoints(lineEquation1, line1, line2, line1.start)
                } else {
                    return Set()
                }
            }
            
        }
    }
}

def getOverlappingPointsNoDiagonal(line1: Line, line2: Line): Set[Point] = {
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

def countOverlappingPoints(lines: List[Line]): Int = {
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
    println(countOverlappingPoints(lines.filterNot(isDiagonal)))
    println(countOverlappingPoints(lines))

}