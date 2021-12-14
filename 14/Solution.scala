import scala.io.Source
type Template = String

type Rules = Map[String, Char]

def genPairs(value: String): List[(Char, Char)] = {
    val pairs = for {
        i <- 0 until value.length - 1
    } yield (value.charAt(i),value.charAt(i + 1))
    pairs.toList
}

def makeNewString(newPairs: List[(Char, Option[Char], Char)]): String = {
    def fromInsertion3n(insertion: (Char, Option[Char], Char)) = insertion._2 match {
        case None => List(insertion._1, insertion._3).mkString
        case Some(c) => List(insertion._1, c, insertion._3).mkString
    }

    def fromInsertion2n(insertion: (Option[Char], Char)) = insertion._1 match {
        case None => insertion._2.toString
        case Some(c) => List(c, insertion._2).mkString
    }

    def reducer(acc: String, insertion: (Char, Option[Char], Char)) = {
        val (a, optional, b) = insertion
        acc + fromInsertion2n((optional, b))
    }
    newPairs.tail.foldLeft(fromInsertion3n(newPairs.head))(reducer)
}

def runStep(template: Template, rules: Rules): Template = {
    val pairs = genPairs(template)
    def updatePair(pair: (Char,Char)): (Char, Option[Char], Char) = {
        val (a, b) = pair
        val key = List(a, b).mkString
        val insertion = rules.get(key)
        (a, insertion, b)
    }
    
    makeNewString(pairs.map(updatePair))
}

def runStepsHelper(template: Template, rules: Rules, noOfSteps: Int): Template = noOfSteps match {
    case 0 => template
    case n => runStepsHelper(runStep(template, rules), rules, n - 1)
}
def runSteps(template: Template, rules: Rules, steps: Int) = {
    runStepsHelper(template, rules, steps)
}

def toTemplateAndRules(text: String): (Template, Rules) = {
    def makeRule(line: String): (String, Char) = {
        val instruction = line.split(" -> ")
        val pair = instruction.head.trim
        val insertion = instruction.tail.head.head
        (pair, insertion)
    }
    val chunks = text.split("\n\r")
    val template = chunks.head.trim
    val rules = chunks.tail.head.split("\r").map(makeRule).toMap
    (template, rules)
}

def calcPart1(template: Template, rules: Rules): Int = {
    val finalString = runSteps(template, rules, 10)
    val groups = finalString.toList.groupBy(identity).view.mapValues(_.length).toList.map(_._2)
    groups.max - groups.min
}

@main def main = {
    val text = Source.fromFile("input.txt").mkString
    val (template, rules) = toTemplateAndRules(text)
    println(calcPart1(template, rules))
}