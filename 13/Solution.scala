import scala.io.Source
type Coord = (Int, Int)
case class FoldInstruction (
    alongX: Boolean,
    value: Int
)
type Paper = Set[Coord]
case class Origami(
    paper: Paper,
    instructions: List[FoldInstruction]
)
type Grid = Vector[Vector[Char]]

def prettyGrid(grid: Grid): String = grid.map((row) => row.mkString).mkString("\n")
def toGrid(paper: Paper): Grid = {
    def updateGrid(grid: Grid, coord: Coord) = {
        val (x, y) = coord
        val oldRow = grid(y)
        grid.updated(y, oldRow.updated(x, '#'))
    }
    // No of rows should be equal to the biggest Y value
    val rowCount = paper.maxBy(_._2)._2 + 1
    val colCount = paper.maxBy(_._1)._1 + 1
    val initialGrid = Vector.fill(rowCount)(Vector.fill(colCount)(' '))
    paper.foldLeft(initialGrid)(updateGrid)
}
def prettyPaper = toGrid andThen prettyGrid
def foldPaper(paper: Paper, instruction: FoldInstruction): Paper = {
    def foldPoint(coord: Coord): Option[Coord] = {
        val (x, y) = coord
        if (instruction.alongX) {
            if (x == instruction.value) {
                None
            } else if (x < instruction.value) {
                Some(coord)
            } else {
                Some((2 * instruction.value - x, y))
            }
        } else {
            if (y == instruction.value) {
                None
            } else if (y < instruction.value) {
                Some(coord)
            } else {
                Some((x, 2 * instruction.value - y))
            }
        }
    }
    paper.map(foldPoint).filter(_.isDefined).map(_.get)
}

def runFolds(origami: Origami): Paper = {
    def runFoldsHelper(paper: Paper, instructions: List[FoldInstruction]): Paper = {
        if (instructions.isEmpty) {
            paper
        } else {
            val instruction :: rest = instructions
            runFoldsHelper(foldPaper(paper, instruction), rest)
        }
    }
    runFoldsHelper(origami.paper, origami.instructions)
}
def makeTuple[A](list: List[A]): (A, A) = (list.head, list.tail.head)

def toFoldInstruction(instruction: String): FoldInstruction = {
    val coordAndVal = instruction
        .split(" ")
        .drop(2)
        .head
        .split("=")
    FoldInstruction(coordAndVal.head == "x", coordAndVal.tail.head.toInt)
}

def toOrigami(text: String): Origami = {
    val chunks = text.split("\n\r")
    val paperCoords = chunks
        .head
        .split("\r")
        .map(line => makeTuple(line.split(",").toList.map(_.trim.toInt)))
        .toSet
    val foldInstructions =  chunks.tail.head.split("\r").map(toFoldInstruction).toList
    Origami(paperCoords, foldInstructions)
}

@main def main = {
    val text = Source.fromFile("input.txt").mkString
    val origami = toOrigami(text)
    val Origami(paper, instructions) = origami
    println(foldPaper(paper, instructions.head).size)
    print(prettyPaper(runFolds(origami)))
}