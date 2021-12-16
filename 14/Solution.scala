import scala.io.Source
import scala.math.BigDecimal.RoundingMode
type Pair = String
type Template = Map[Pair, Long]
type Head = String
type Tail = String
type Rules = Map[String, Char]

def genPairs(value: String): List[String] = {
    val pairs = for {
        i <- 0 until value.length - 1
    } yield value.substring(i, i + 2)
    pairs.toList
}

def runStep(template: Template, rules: Rules): Template = {
    def updateMap(acc: Template, key: String) = {
        // Check Key is mapped to rule
        val insertion = rules.get(key)
        val currCount = template.getOrElse(key, 0l)
        if (insertion.isEmpty) {
            acc.updatedWith(key)(_.map(_ + currCount).orElse(Some(currCount)))
        } else {
            val (a, b) = (key.charAt(0), key.charAt(1))
            val (k1, k2) = (List(a, insertion.get).mkString, List(insertion.get, b).mkString)
            acc
                .updatedWith(k1)(_.map(_ + currCount).orElse(Some(currCount)))
                .updatedWith(k2)(_.map(_ + currCount).orElse(Some(currCount)))
        }
    }
    template.keySet.foldLeft(Map.empty)(updateMap)
}

def runSteps(template: Template, rules: Rules, steps: Int): Template= steps match {
        case 0 => template
        case n => {
            runSteps(runStep(template, rules), rules, n - 1)
        }
    }

def makeTemplate(value: String): Template = {
    val pairs =  genPairs(value)
    def updateTemplate(acc: Template, pair: String): Template = {
        acc.updatedWith(pair)(_.map(_ + 1l).orElse(Some(1l)))
    }
    pairs.foldLeft(Map.empty)(updateTemplate)
}

def toTemplateAndRules(text: String): (Template, Rules) = {
    def makeRule(line: String): (String, Char) = {
        val instruction = line.split(" -> ")
        val pair = instruction.head.trim
        val insertion = instruction.tail.head.head
        (pair, insertion)
    }
    val chunks = text.split("\n\r")
    val template = makeTemplate(chunks.head.trim)
    val rules = chunks.tail.head.split("\r").map(makeRule).toMap
    (template, rules)
}

def makeCharMap(template: Template): Map[Char, BigInt] = {
    def reducer(acc: Map[Char, BigDecimal], pair: String) = {
        val count = BigDecimal(template.getOrElse(pair, 0l))
        acc
            .updatedWith(pair.charAt(0))(_.map(_ + count).orElse(Some(count)))
            .updatedWith(pair.charAt(1))(_.map(_ + count).orElse(Some(count)))
    }
    template.keySet.foldLeft(Map.empty)(reducer).map( (k, v) => (k, (v/BigDecimal(2.0)).setScale(0, RoundingMode.CEILING).toBigInt))
}

def calcSolution(template: Template, rules: Rules, steps: Int) = {
    val finalTemplate = runSteps(template, rules, steps)
    val finalCharMap = makeCharMap(finalTemplate)
    val maxGroup = finalCharMap.maxBy(_._2)._2
    val minGroup = finalCharMap.minBy(_._2)._2
    maxGroup - minGroup
}

def calcPart1(template: Template, rules: Rules): BigInt = {
    calcSolution(template, rules, 10)
}

def calcPart2(template: Template, rules: Rules): BigInt = {
    calcSolution(template, rules, 40)
}

@main def main = {
    val text = Source.fromFile("input.txt").mkString
    val (template, rules) = toTemplateAndRules(text)
    val t0 = System.nanoTime()
    val ans = calcPart2(template, rules)
    val t1 = System.nanoTime()
    println(ans)
    println(s"Runtime: ${(t1 - t0)/ 1000000}ms")
}