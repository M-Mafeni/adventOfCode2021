import scala.io.Source
type Digit = String
type OutputValue = List[Digit]
type SignalMap = Map[Digit, Int]
type DigitMap = Map[Char, Boolean]
type Wire = Char
type Segment = Char
// true signal mapped to Wire being displayed
type SegmentMap = Map[Segment, Wire]

case class Entry (
    uniqueSamples: List[Digit],
    outputValue: OutputValue
)

val example = toEntry( "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

def getSignalMap(uniqueSamples: List[Digit]): SignalMap = {
    val uniqueSamplesDigits = uniqueSamples.map(_.toSet)

    val digitsFrom1 = uniqueSamplesDigits.filter(_.size == 2).head
    val digitsFrom4 = uniqueSamplesDigits.filter(_.size == 4).head
    val digitsFrom7 = uniqueSamplesDigits.filter(_.size == 3).head
    val digitsFrom8 = uniqueSamplesDigits.filter(_.size == 7).head
    val digitsFrom4diff1 = digitsFrom4 -- digitsFrom1
    // Find sample for 2
    val digitsFrom2 = uniqueSamplesDigits
        .filter((set) => set.size == 5 && (set & digitsFrom1).size == 1 && (set & digitsFrom4diff1).size == 1)
        .head
    val digitsFrom0 = uniqueSamplesDigits
        .filter((set) => set.size == 6 && (set & (digitsFrom4 & (digitsFrom2 -- digitsFrom1))).size == 0)
        .head
    val wireForC = (digitsFrom2 & digitsFrom1).toList.head
    val digitsFrom6 = uniqueSamplesDigits
        .filter((set) => set.size == 6 && !set.contains(wireForC))
        .head
    val digitsFrom9 = uniqueSamplesDigits
        .filter((set) => set.size == 6 && !(set == digitsFrom6 || set == digitsFrom0))
        .head
    val wireForE = ("abcdefg".toSet -- digitsFrom9).toList.head
    val digitsFrom5 = uniqueSamplesDigits
        .filter((set) => set.size == 5 && !(set.contains(wireForE) || set.contains(wireForC)))
        .head
    val digitsFrom3 = uniqueSamplesDigits
        .filter((set) => set.size == 5 && !(set == digitsFrom2 || set == digitsFrom5))
        .head
    List(
        (0, digitsFrom0),
        (1, digitsFrom1),
        (2, digitsFrom2),
        (3, digitsFrom3),
        (4, digitsFrom4),
        (5, digitsFrom5),
        (6, digitsFrom6),
        (7, digitsFrom7),
        (8, digitsFrom8),
        (9, digitsFrom9)
    ).map((n, set) => (set.toList.sorted.mkString, n))
    .toMap

}

def toNum(outputValue: OutputValue, signalMap: SignalMap): Int = outputValue.map((digit) => ('0' + signalMap.getOrElse(digit, -1)).toChar).mkString.toInt

def fromEntry(entry: Entry): Int = toNum(entry.outputValue, getSignalMap(entry.uniqueSamples))
def uniqueCount(entry: Entry): Int = {
    // segment for 1 would be of length 2, 4 length 4, 7 length 3 and 8 length 7
    def isUnique(segment: Digit): Boolean = List(2,4,3,7).contains(segment.length)
    entry.outputValue.filter(isUnique).length
}

def toEntry(text: String): Entry = {
    val values = text.split(" \\| ").toList
    val uniqueSamples = values.head.split(" ").toList.map(_.sorted)
    val outputValue = values.tail.head.split(" ").toList.map(_.sorted)
    Entry(
        uniqueSamples,
        outputValue
    )
}

@main def main = {
    val entries = Source.fromFile("input.txt").getLines.toList.map(toEntry)
    println(entries.map(uniqueCount).sum)
    println(entries.map(fromEntry).sum)
}
