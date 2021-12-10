import scala.io.Source
type LanternFish = Int
// Map of timer to fish count
type LanternFishMap = Map[Int, Long]

def ageByADay(fishcountMap: LanternFishMap): LanternFishMap = {
    def newEntries = List.range(0,9).map((key) => key match {
            // case 0 => (0, fishcountMap.getOrElse(1, 0))
            // case 1 => (1, fishcountMap.getOrElse(2, 0))
            // new 6 count should be old zero count + old seven count
            case 6 => (6, fishcountMap.getOrElse(0, 0.toLong) + fishcountMap.getOrElse(7, 0.toLong)) 
            // new 8 count should be old zero count
            case 8 => (8, fishcountMap.getOrElse(0, 0.toLong))
            // Every other day should get the day that was ahead of it
            case n => (n, fishcountMap.getOrElse(n + 1, 0.toLong))
       })
    newEntries.toMap
}

def ageByDays(fishCountMap: LanternFishMap, days: Int): LanternFishMap = if (days == 0) then fishCountMap else ageByDays(ageByADay(fishCountMap), days - 1)

def toLanternFishMap(fishes: List[LanternFish]): LanternFishMap = fishes.groupBy(identity).view.mapValues(_.length.toLong).toMap

def getLanternFishCount(fishCountMap: LanternFishMap): Long = fishCountMap.values.sum

def getLanternFishCountAfterDays(fishCountMap: LanternFishMap, days: Int) = getLanternFishCount(ageByDays(fishCountMap, days))

@main def main = {
    val fishes: List[LanternFish] = Source.fromFile("input.txt").mkString.split(",").map(_.toInt).toList
    println(getLanternFishCountAfterDays(toLanternFishMap(fishes), 80))
    println(getLanternFishCountAfterDays(toLanternFishMap(fishes), 256))
}