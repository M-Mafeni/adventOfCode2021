import scala.io.Source

type GameOrder = List[Int]
type Marked = Boolean
type Board = Vector[Vector[(Int, Marked)]]
case class GameState (
    gameOrder: GameOrder,
    boards: List[Board]
)

def makeMove(gameState: GameState): (Option[Board], Int, GameState) = {
    val numToMark = gameState.gameOrder.head
    def markBoardVal(pos: (Int,Marked)) = if (pos._1 == numToMark) (pos._1, true) else pos
    val markedBoards = gameState
        .boards
        .map(_.map(_.map(markBoardVal)))
    val winningBoard = markedBoards.find(hasWon)
    return (winningBoard, numToMark, GameState(gameState.gameOrder.tail, markedBoards))
}

def runGame(gameState: GameState): (Board, Int) = {
    val (winner, numToMark, newState) = makeMove(gameState)
    if (winner.isEmpty) {
        return runGame(newState)
    } else {
        return (winner.get, numToMark)
    }
}

def makeBadMove(gameState: GameState): (Option[Board], Int, GameState) = {
    val numToMark = gameState.gameOrder.head
    def markBoardVal(pos: (Int,Marked)) = if (pos._1 == numToMark) (pos._1, true) else pos
    val markedBoards = gameState
        .boards
        .map(_.map(_.map(markBoardVal)))
    val winningBoard = markedBoards.find(hasWon)
    val unwonBoards = markedBoards.filterNot(hasWon)
    // If this is the last board return it
    if (!winningBoard.isEmpty && unwonBoards.length == 0) {
        return (winningBoard, numToMark, GameState(gameState.gameOrder.tail, unwonBoards))
    } 

    return (Option.empty, numToMark, GameState(gameState.gameOrder.tail, unwonBoards))
}

def runBadGame(gameState: GameState): (Board, Int) = {
    val (winner, numToMark, newState) = makeBadMove(gameState)
    if (winner.isEmpty) {
        return runBadGame(newState)
    } else {
        return (winner.get, numToMark)
    }
}

def getInitGameState(filename: String): GameState = {
    def getGameOrder(line: String): GameOrder = line.split(",").map(_.filter(_ >= ' ').toInt).toList
    def getBoard(boardString: String): Board = {
        val rows = boardString.split("\r").toVector
        return rows.map(_.split(" ").filterNot(_.filter(_ >= ' ').isEmpty).map(num => (num.filter(_ >= ' ').toInt, false)).toVector)
        return Vector()
    }

    val content = Source.fromFile(filename).mkString
    val rows = content.split("\n\r").toList
    val gameOrder = getGameOrder(rows.head)
    val boards = rows.tail.map(getBoard)

    return GameState(gameOrder, boards)
}

def hasWon(board: Board): Boolean = {
    def hasWonRow = board.exists((row) => row.forall((x, marked) => marked))
    def hasWonCol = board.transpose.exists((row) => row.forall((x, marked) => marked))
    return hasWonRow || hasWonCol
}

def runHasWonTests = {
    val testBoard: Board = Vector(
        Vector((1, true),(2, true),(3, true)),
        Vector((4, false),(5, false),(6, false)),
        Vector((7, false),(8, false),(9, false))
    )

    // test has Won by row 
    if (!hasWon(testBoard)) {
        println("Has Won by row failed")
    }

    val testBoard2: Board = Vector(
        Vector((1, true),(2, false),(3, false)),
        Vector((4, true),(5, false),(6, false)),
        Vector((7, true),(8, false),(9, false))
    )

    // test has Won by col
    if (!hasWon(testBoard2)) {
        println("Has Won by col failed")
    }

    val testBoard3: Board = Vector(
        Vector((1, false),(2, false),(3, true)),
        Vector((4, false),(5, true),(6, false)),
        Vector((7, true),(8, false),(9, false))
    )

    // test not has Won
    if (hasWon(testBoard3)) {
        println("Not Has Won check failed")
    }
}

def calcWinningScore(board: Board, numMarked: Int): Int = {
    val unMarkedSum = board.map(_.filter(!_._2).foldLeft(0)((acc, pos) => acc + pos._1)).sum
    return unMarkedSum * numMarked
}

def runTests = {
    runHasWonTests
}

@main def main = {
    runTests
    val initialGameState = getInitGameState("input.txt")
    println((runGame andThen calcWinningScore)(initialGameState))
    println((runBadGame andThen calcWinningScore)(initialGameState))

}