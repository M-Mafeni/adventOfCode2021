import scala.compiletime.ops.string
import scala.io.Source
enum BracketType:
    case Normal, Square, Curly, Angled

case class Bracket (
    bracketType: BracketType,
    open: Boolean
)
type Brackets = List[Bracket]

def toBracket(x: Char): Bracket = x match {
    case '(' => Bracket(BracketType.Normal, true)
    case ')' => Bracket(BracketType.Normal, false)
    case '[' => Bracket(BracketType.Square, true)
    case ']' => Bracket(BracketType.Square, false)
    case '{' => Bracket(BracketType.Curly, true)
    case '}' => Bracket(BracketType.Curly, false)
    case '<' => Bracket(BracketType.Angled, true)
    case '>' => Bracket(BracketType.Angled, false)
}

def toBracketType(x: Char): BracketType = x match {
    case '(' | ')' => BracketType.Normal
    case '[' | ']' => BracketType.Square
    case '{' | '}' => BracketType.Curly
    case '<' | '>' => BracketType.Angled
}

def toBracketTypes(text: String) = text.toList.map(toBracketType)
def getCorruptedCharHelper(brackets: List[Bracket], openedBrackets: List[Bracket]): Option[BracketType]  = {
    if (brackets.isEmpty) {
        Option.empty
    } else {
        val bracket::rest =  brackets
        // Update counts
        if (bracket.open) {
            getCorruptedCharHelper(rest, bracket::openedBrackets)
        } else {
            val opening = openedBrackets.headOption
            if (opening.isEmpty || opening.get.bracketType != bracket.bracketType) {
                Option(bracket.bracketType)
            } else {
                getCorruptedCharHelper(rest, openedBrackets.tail)
            }
        }
    }
}

def getCorruptedChar(brackets: List[Bracket]): Option[BracketType] = {
    getCorruptedCharHelper(brackets, List())
}

def scoreCorruptedChar(corruptedChar: Option[BracketType]): Int = corruptedChar match {
    case None =>  0
    case Some(BracketType.Normal) => 3
    case Some(BracketType.Square) => 57
    case Some(BracketType.Curly) => 1197
    case Some(BracketType.Angled) => 25137
}

def getCorruptedCharFromText(text: String): Option[BracketType] = (getCorruptedChar compose toBrackets)(text)
def getBracketsToCompleteFromText(text: String): List[BracketType] = (getBracketsToComplete compose toBrackets)(text)
def getCorruptedCharTest = {
    assert(getCorruptedCharFromText("{([(<{}[<>[]}>{[]{[(<()") == Option(BracketType.Curly))
    assert(getCorruptedCharFromText("[[<[([]))<([[{}[[()]]]") == Option(BracketType.Normal))
    assert(getCorruptedCharFromText("[{[{({}]{}}([{[{{{}}([]") == Option(BracketType.Square))
    assert(getCorruptedCharFromText("[<(<(<(<{}))><([]([]()") == Option(BracketType.Normal))
    assert(getCorruptedCharFromText("<{([([[(<>()){}]>(<<{{") == Option(BracketType.Angled))
}

def getBracketsToCompleteTest = {
    assert(getBracketsToCompleteFromText("[({(<(())[]>[[{[]{<()<>>") == toBracketTypes("}}]])})]"))
    assert(getBracketsToCompleteFromText("[(()[<>])]({[<{<<[]>>(") == toBracketTypes(")}>]})"))
    assert(getBracketsToCompleteFromText("(((({<>}<{<{<>}{[]{[]{}") == toBracketTypes("}}>}>))))"))
    assert(getBracketsToCompleteFromText("{<[[]]>}<{[{[{[]{()[[[]") == toBracketTypes("]]}}]}]}>"))
    assert(getBracketsToCompleteFromText("<{([{{}}[<[[[<>{}]]]>[]]") == toBracketTypes("])}>"))
    assert(getBracketsToCompleteFromText("()[") == toBracketTypes("]"))
    assert(getBracketsToCompleteFromText("()[]<") == toBracketTypes(">"))
    assert(getBracketsToCompleteFromText("<(()") == toBracketTypes(")>"))
    assert(getBracketsToCompleteFromText("<({}") == toBracketTypes(")>"))
    assert(getBracketsToCompleteFromText("<({})[][][(") == toBracketTypes(")]>"))


}

def toBrackets(text: String): List[Bracket] = text.toList.map(toBracket(_))
def isCorrupted(brackets: Brackets): Boolean = !getCorruptedChar(brackets).isEmpty
def getCorruptedScoring(lines: List[Brackets]): Int = lines.map(scoreCorruptedChar compose getCorruptedChar).sum
def getIncompleteScoring(incomplete: List[BracketType]): Long = {
    def getIncompleteScore(bracketType: BracketType): Long = bracketType match {
        case BracketType.Normal => 1
        case BracketType.Square => 2
        case BracketType.Curly => 3
        case BracketType.Angled => 4
    }
    incomplete.foldLeft(0.toLong)((acc, bracketType) => acc * 5.toLong + getIncompleteScore(bracketType))
}
def bracketsToCompleteHelper(brackets: List[Bracket], openedBrackets: List[Bracket]): List[BracketType] = {
    if (brackets.isEmpty) {
        openedBrackets.map(_.bracketType)
    } else {
        val bracket :: rest = brackets
        if (bracket.open) {
            bracketsToCompleteHelper(rest, bracket:: openedBrackets)
        } else {
            bracketsToCompleteHelper(rest, openedBrackets.tail)
        }
    }
}
def getBracketsToComplete(brackets: Brackets): List[BracketType] = {
    bracketsToCompleteHelper(brackets, List())
}

def getMiddleScore(lines: List[Brackets]): Long = {
    val scores = lines.toVector.map(getBracketsToComplete andThen getIncompleteScoring).sorted
    // Assume odd number of scores
    assert(scores.length % 2 == 1)
    scores(scores.length / 2)
}
@main def main = {
    getCorruptedCharTest
    val lines = Source.fromFile("input.txt").getLines.toList.map(toBrackets)
    println(getCorruptedScoring(lines))
    println(getMiddleScore(lines.filterNot(isCorrupted)))
}