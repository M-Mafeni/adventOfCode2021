import scala.io.Source

enum Direction:
    case Forward, Up, Down

case class Move(
    direction: Direction,
    value: Int
)

case class Position (
    horizontal: Int,
    depth: Int
)

case class PositionWithAim (
    horizontal: Int,
    depth: Int,
    aim: Int
)

// Make move on pos and return a new move
def makeMoveAim(pos: PositionWithAim, move: Move): PositionWithAim = move match
    case Move(Direction.Forward, value) => PositionWithAim(pos.horizontal + value, pos.depth + pos.aim * value, pos.aim)
    case Move(Direction.Up, value) => PositionWithAim(pos.horizontal, pos.depth, pos.aim - value)
    case Move(Direction.Down, value) => PositionWithAim(pos.horizontal, pos.depth, pos.aim + value)

// Make move on pos and return a new move
def makeMove(pos: Position, move: Move): Position = move match
    case Move(Direction.Forward, value) => Position(pos.horizontal + value, pos.depth)
    case Move(Direction.Up, value) => Position(pos.horizontal, pos.depth - value)
    case Move(Direction.Down, value) => Position(pos.horizontal, pos.depth + value)

def makeMoves(startPos: Position, moves: List[Move]): Position = 
    moves.foldLeft(startPos)(makeMove)

def makeMovesAim(startPos: PositionWithAim, moves: List[Move]): PositionWithAim = 
    moves.foldLeft(startPos)(makeMoveAim)

def parseMove(text: String): Move = {
    val values = text.split(" ")
    val dirText = values(0)
    val num = values(1)
    val direction: Direction = dirText match
        case "forward" => Direction.Forward
        case "up" => Direction.Up
        case "down" => Direction.Down
    return Move(direction, num.toInt)
}

object Solution {
    def main() = {
        val filename = "input.txt"
        val bufferedSource = Source.fromFile(filename)
        val moves = bufferedSource.getLines.toList.map(parseMove)
        val finalPos = makeMoves(Position(0,0), moves)
        val finalPosAim = makeMovesAim(PositionWithAim(0,0,0), moves)

        println(finalPos.depth * finalPos.horizontal)
        println(finalPosAim.depth * finalPosAim.horizontal)

    }
}