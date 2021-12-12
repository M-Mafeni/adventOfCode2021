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

@main def main = {
    val lines = Source.fromFile("input.txt").getLines.toList
    val graph = toGraph(lines)
    println(getPaths(graph).size)
}