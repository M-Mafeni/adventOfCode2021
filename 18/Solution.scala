object Solution {
    

type PairElem = Either [Int, SnailFish]
case class SnailFish (
    left: PairElem,
    right: PairElem
)

type SnailFishZipper = (PairElem, List[Crumb])
type Crumb = LeftCrumb | RightCrumb
// enum Direction:
//     case Left, Right
object Direction extends Enumeration {
    val Left, Right = Value
}
case class LeftCrumb (
    value: PairElem
)
case class RightCrumb ( 
    value: PairElem
)

def fromSnailFishZipper(zipper: SnailFishZipper): SnailFish = {
    def addCrumb(acc: SnailFish, p: Crumb): SnailFish = p match {
        case LeftCrumb(value) => SnailFish(value, acc)
        case RightCrumb(value) => SnailFish(acc, value)
    }
    val (pairElem, crumbs) = zipper
    pairElem match {
        case SnailFish(left, right) => {
            if (crumbs.isEmpty) {
                SnailFish(left, right)
            } else {
                crumbs.foldLeft(SnailFish(left, right))(addCrumb)
            }
        }
        case n => {
            if (!crumbs.isEmpty) {
                val crumb :: rest = crumbs
                val startSnailFish = crumb match {
                    case LeftCrumb(value) => SnailFish(value, n)
                    case RightCrumb(value) => SnailFish(n, value)
                }
                crumbs.tail.foldLeft(startSnailFish)(addCrumb)
            } else {
                SnailFish(-1, -1)
            }
        }

    }
    
}

def doMoves(zipper: SnailFishZipper, directions: List[Direction]): Option[SnailFishZipper] = {
    def move(acc: Option[SnailFishZipper], direction: Direction) = direction match {
        case Direction.Left => acc.flatMap(goLeft)
        case Direction.Right => acc.flatMap(goRight)
    }
    directions.foldLeft(Some(zipper))(move)
}
def toSnailFishZipper(snailFish: SnailFish): SnailFishZipper = (snailFish, List())
def goRight(zipper: SnailFishZipper): Option[SnailFishZipper] = {
    val (pairElem, crumbs) = zipper
    pairElem match {
        case SnailFish(left, right) => Some((right, LeftCrumb(left) :: crumbs))
        case n => None
    }
}

def goLeft(zipper: SnailFishZipper): Option[SnailFishZipper] = {
    val (pairElem, crumbs) = zipper
    pairElem match {
        case SnailFish(left, right) => Some((left, RightCrumb(right) :: crumbs))
        case n => None
    }
}

def goBack(zipper: SnailFishZipper): Option[SnailFishZipper] = {
    val (pairElem, crumbs) = zipper
    crumbs.headOption match {
        case Some(LeftCrumb(value)) => Some((SnailFish(value, pairElem), crumbs.tail))
        case Some(RightCrumb(value)) => Some((SnailFish(pairElem, value), crumbs.tail))
        case None => None
    }
}


val example = SnailFish(
    SnailFish( 
        SnailFish(
            SnailFish(1,2),
            SnailFish(3,4)
        ),
        SnailFish(
            SnailFish(5,6),
            SnailFish(7,8)
        )
    ),
    9
)

val explodeExample1 = SnailFish(
    SnailFish(
        SnailFish(
            SnailFish(
                SnailFish(9,8),
                1
            ),
            2
        ),
        3
    ),
    4
)

val explodeExample2 = SnailFish(
    7,
    SnailFish(
        6,
        SnailFish(
            5,
            SnailFish(
                4,
                SnailFish(3,2)
            )
        )
    )
)

val explodeExample3 = SnailFish(
    SnailFish(
        6,
        SnailFish(5,
            SnailFish(
                4,
                SnailFish(3,2)
            ),
        ),
    ),
    1
)

// [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
val explodeExample4 = SnailFish(
    SnailFish(3, SnailFish(2, SnailFish(1, SnailFish(7,3)))),
    SnailFish(6, SnailFish(5, SnailFish(4, SnailFish(3,2)))),
)

// [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
val explodeExample5 = SnailFish(SnailFish(3,SnailFish(2,SnailFish(8,0))),SnailFish(9,SnailFish(5,SnailFish(4,SnailFish(3,2)))))

def addSnailFish(s1: SnailFish, s2: SnailFish): SnailFish = SnailFish(s1, s2)

def addAndReduceSnailFish(s1: SnailFish, s2: SnailFish) = runReduction(SnailFish(s1, s2)) 

val possLeftDirections = {
    val directions = List(Direction.Left, Direction.Right)
    for {
        a <- directions
        b <- directions
        c <- directions
        d <- directions
    } yield(List(a,b,c,d))
}

def getDirectionsToPairFourLevelsInHelper(zipper: SnailFishZipper, directionsList: List[List[Direction]]): Option[List[Direction]] = {
    if (directionsList.isEmpty) {
        None
    } else {
        val directions :: rest = directionsList
        val optionZipper = doMoves(zipper, directions)
        optionZipper match {
            case Some(SnailFish(x: Int, y: Int), _) => Some(directions)
            case _ => getDirectionsToPairFourLevelsInHelper(zipper, rest)
        }
    }

}
def getDirectionsToPairFourLevelsIn(zipper: SnailFishZipper): Option[List[Direction]] = getDirectionsToPairFourLevelsInHelper(zipper, possLeftDirections)


def incrementLeftSnailFish(snailFish: SnailFish, increment: Int): SnailFish = snailFish match {
    case SnailFish(a, b: Int) => SnailFish(a, b + increment)
    case SnailFish(a, b: SnailFish) => SnailFish(a, incrementLeftSnailFish(b, increment))
}
def updateFirstLeftCrumb(crumbs: List[Crumb], increment: Int): List[Crumb] = crumbs match {
    case List() => List()
    case x :: xs => x match {
       case RightCrumb(_) => x :: updateFirstLeftCrumb(xs, increment)
       case LeftCrumb(value: Int) => LeftCrumb(value + increment) :: xs
       case LeftCrumb(value: SnailFish) => LeftCrumb(incrementLeftSnailFish(value, increment)) :: xs
    }
}

def incrementRightSnailFish(snailFish: SnailFish, increment: Int): SnailFish = snailFish match {
    case SnailFish(a: Int, b) => SnailFish(a + increment , b)
    case SnailFish(a: SnailFish, b) => SnailFish(incrementRightSnailFish(a, increment), b)
}
def updateFirstRightCrumb(crumbs: List[Crumb], increment: Int): List[Crumb] = crumbs match {
    case List() => List()
    case x :: xs => x match {
       case LeftCrumb(_) => x :: updateFirstRightCrumb(xs, increment)
       case RightCrumb(value: Int) => RightCrumb(value + increment) :: xs
       case RightCrumb(value: SnailFish) =>RightCrumb(incrementRightSnailFish(value, increment)) :: xs
    }
}
def explodeSnailFishHelper(zipper: SnailFishZipper, directions: List[Direction]): SnailFishZipper = {
    val optionZipper = doMoves(zipper, directions)
    val (elem, crumbs) = optionZipper.get
    elem match {
        case SnailFish(x: Int, y: Int) => {
            val updatedLeftCrumbs = updateFirstLeftCrumb(crumbs, x)
            val updatedCrumbs = updateFirstRightCrumb(updatedLeftCrumbs, y)
            (0, updatedCrumbs)
        }
        case _ => zipper
    }
    
}

def explodeSnailFish(snailFish: SnailFish): (SnailFish, Boolean) = {
    val zipper = toSnailFishZipper(snailFish)
    val directionsOption = getDirectionsToPairFourLevelsIn(zipper)
    directionsOption match {
        case None => (snailFish, false)
        case Some(directions) => (fromSnailFishZipper(explodeSnailFishHelper(zipper, directions)), true)
    }
}

def snailFishToListHelper(pairElem: PairElem): List[Int] = {
    pairElem match {
        case x: Int => List(x)
        case SnailFish(x, y) => snailFishToListHelper(x) ::: snailFishToListHelper(y)
    }
}

def snailFishToList(snailFish: SnailFish): List[Int] = {
    val leftElems = snailFishToListHelper(snailFish.left)
    val rightElems = snailFishToListHelper(snailFish.right)
    leftElems ::: rightElems
}

def searchLeft(zipperOption: Option[SnailFishZipper]): Option[Int] = {
    if (zipperOption.isEmpty) {
        None
    } else {
        val (elem, _) = zipperOption.get
        elem match {
            case x: Int => Some(x)
            case n => {
                val leftIntOption = searchLeft(zipperOption.flatMap(goLeft))
                leftIntOption
                    .orElse(searchLeft(zipperOption.flatMap(goRight)))
    
            }
        }
    }
}

def getLeftMostInt(snailFish: SnailFish): Int = {
    val zipper = toSnailFishZipper(snailFish)
    searchLeft(Some(zipper)).get
}

def goToLeftMostIntGreaterThan10Helper(zipperOption: Option[SnailFishZipper]): Option[SnailFishZipper] = {
    if (zipperOption.isEmpty) {
        None
    } else {
        val (elem, _) = zipperOption.get
        elem match {
            case x: Int => if (x > 10) then zipperOption else None
            case n => {
                val leftIntOption = goToLeftMostIntGreaterThan10Helper(zipperOption.flatMap(goLeft))
                leftIntOption
                    .orElse(goToLeftMostIntGreaterThan10Helper(zipperOption.flatMap(goRight)))
    
            }
        }
    }
}

// [[[[0,7],4], [15,[0,13]]], [1,1]]
val splitExample = SnailFish(
    SnailFish(
        SnailFish(SnailFish(0,7), 4),
        SnailFish(15, SnailFish(0, 13))
    ),
    SnailFish(1,1)
)
def goToLeftMostIntgreaterThan10(snailFish: SnailFish): Option[SnailFishZipper] = {
    val zipper = toSnailFishZipper(snailFish)
    goToLeftMostIntGreaterThan10Helper(Some(zipper))
}

def splitSnailFish(snailFish: SnailFish): (SnailFish, Boolean) = {
    val zipper = goToLeftMostIntgreaterThan10(snailFish)
    zipper match {
        case Some(elem: Int, crumbs) => {
            val halved = elem / 2.0
            val newPair = SnailFish(halved.floor.toInt, halved.ceil.toInt)
            val newZipper = (newPair, crumbs)
            (fromSnailFishZipper(newZipper), true)
        }
        case _ => (snailFish, false)
    }
}

def runReduction(snailFish: SnailFish): SnailFish = {
    val (explodedSnailFish, explodeActionRan) = explodeSnailFish(snailFish)
    val (splittedSnailFish, splitActionRan) = splitSnailFish(explodedSnailFish)
    (!splitActionRan && !explodeActionRan) match {
        case true => splittedSnailFish
        case _    => runReduction(splittedSnailFish)
    }
}

def reduceSnailFishList(snailFishes: List[SnailFish]): SnailFish = snailFishes.reduceLeft(addAndReduceSnailFish)
def calcMagnitude(snailFish: SnailFish): Int = snailFish match {
    case SnailFish(x: Int, y: Int) => 3 * x + 2 * y
    case SnailFish(x: Int, y: SnailFish) => 3 * x + 2 * calcMagnitude(y)
    case SnailFish(x: SnailFish, y: Int) => 3 * calcMagnitude(x) + 2 * y
    case SnailFish(x: SnailFish, y: SnailFish) => 3 * calcMagnitude(x) + 2 * calcMagnitude(y)
}
// [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
val reductionExample = SnailFish(
    SnailFish(
        SnailFish(SnailFish(SnailFish(4, 3), 4), 4),
        SnailFish(7, SnailFish(SnailFish(8, 4), 9))
    ),
    SnailFish(1,1)
)

def calcFinalSum = reduceSnailFishList andThen calcMagnitude

// def parseSnailFish(text: String): SnailFish = {
//     val removedBrackets =  text.substring(1, text.length - 1)

// }
}