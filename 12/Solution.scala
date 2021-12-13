import scala.io.Source
type Cave = String
type Graph = Map[Cave, Set[Cave]]
type Path = List[Cave]

def isSmallCave(cave: Cave) = cave.head.isLower 
def toGraph(lines: List[String]): Graph = {
    def helper(acc: Graph, line: String) = {
        val nodes = line.split("-").toList
        val source = nodes.head
        val dest = nodes.tail.head
        val newSourceNeighbours = acc.getOrElse(source, Set.empty) + dest
        val newDestNeighbours = acc.getOrElse(dest, Set.empty) + source
        acc.updated(source, newSourceNeighbours).updated(dest, newDestNeighbours)
    }
    lines.foldLeft(Map.empty)(helper)
}

def getPaths(graph: Graph): Set[Path] = {
    def getPathHelper(source: Cave, visited: Set[Cave], currPath: Path): Set[Path] = {
        if (source == "end") {
            Set((source :: currPath).reverse)
        } else {
            val neighbours = graph.getOrElse(source, Set.empty) -- visited
            val updatedVisited = if isSmallCave(source) then visited + source else visited
            val optionalPaths = neighbours.map(cave => getPathHelper(cave, updatedVisited, source :: currPath)).reduceLeftOption(_ | _)
            if optionalPaths.isEmpty then Set.empty else optionalPaths.get
        }
    }
    getPathHelper("start", Set.empty, List())
}


def getPaths2(graph: Graph): Set[Path] = {
    // Update vistied and check if path is still valid
    def updateVisited(visited: Map[Cave, Int], cave: Cave): Option[Map[Cave, Int]] = {
        if (!isSmallCave(cave)) {
            Some(visited)
        } else {
            /* If a cave has already been visited twice (2xCave)
                If (cave == 2xCave || cave has already been visited) return failure string // can't visit cave any more times
                else add to Map
               else Add to Map
            */
            val optionalTwiceCave = visited.find((_, count) => count == 2).map(_._1)
            optionalTwiceCave match {
               case None => Some(visited.updated(cave, visited.getOrElse(cave, 0) + 1))
               case Some(twiceCave) => if (cave == twiceCave || visited.get(cave).isDefined) None else Some(visited.updated(cave, visited.getOrElse(cave, 0) + 1))
            }
        }
    }
    def getPathHelper(source: Cave, visited: Map[Cave, Int], currPath: Path): Set[Path] = {
        if (source == "start" && !currPath.isEmpty) {
            // Invalid Path
            Set.empty
        } else if (source == "end") {
            Set((source :: currPath).reverse)
        } else {
            val neighbours = graph.getOrElse(source, Set.empty)
            val optionalUpdatedVisited = updateVisited(visited, source)
            optionalUpdatedVisited match {
                case None => Set.empty
                case Some (newVisited) => {
                    val optionalPaths = neighbours.map(cave => getPathHelper(cave, newVisited, source :: currPath)).reduceLeftOption(_ | _)
                    if optionalPaths.isEmpty then Set.empty else optionalPaths.get
                }
            }

        }
    }
    getPathHelper("start", Map.empty, List())
}

@main def main = {
    val lines = Source.fromFile("input.txt").getLines.toList
    val graph = toGraph(lines)
    println(getPaths(graph).size)
    println(getPaths2(graph).size)
}