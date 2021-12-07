import scala.io.Source

def binaryToDecimal(binString: String): Int = {
    val exponents = List.range(0, binString.length).map(_.toDouble).reverse
    val bits = binString.toList
    val decimals = (bits lazyZip exponents).map((b, exp) => if (b == '1') scala.math.pow(2, exp).toInt else 0)
    return decimals.sum
}

def makeIndexMap(binaryStrings: List[List[Char]]): Map[Int, Vector[Char]] = List.range(0, binaryStrings.length).zip(binaryStrings.map(_.toVector)).toMap

def calcOxygenRate(binaryStrings: List[List[Char]]): Int = {

    def oxygenHelper(bitPosition: Int, indexMap: Map[Int, Vector[Char]]): String = {
        if (indexMap.keySet.size == 1) {
            return indexMap.values.toList.head.mkString
        }
        val binVectors = indexMap.values.toList
        val bitsAtBitPos = binVectors.map(_(bitPosition))
        val oneCount = bitsAtBitPos.count(_ == '1')
        val mostCommonIsOne = oneCount >= bitsAtBitPos.length - oneCount
        val filteredMap = indexMap.filter((_, binVector) => if (mostCommonIsOne) binVector(bitPosition) == '1' else binVector(bitPosition) == '0')
        return oxygenHelper(bitPosition + 1, filteredMap)
    }

    val oxygenRateString = oxygenHelper(0, makeIndexMap(binaryStrings))
    return binaryToDecimal(oxygenRateString)
}

def calcC02Rate(binaryStrings: List[List[Char]]): Int = {
    def co2Helper(bitPosition: Int, indexMap: Map[Int, Vector[Char]]): String = {
        if (indexMap.keySet.size == 1) {
            return indexMap.values.toList.head.mkString
        }
        val binVectors = indexMap.values.toList
        val bitsAtBitPos = binVectors.map(_(bitPosition))
        val oneCount = bitsAtBitPos.count(_ == '1')
        val mostCommonIsOne = oneCount >= bitsAtBitPos.length - oneCount
        val filteredMap = indexMap.filter((_, binVector) => if (mostCommonIsOne) binVector(bitPosition) == '0' else binVector(bitPosition) == '1')
        return co2Helper(bitPosition + 1, filteredMap)
    }
    val co2ScrubberString = co2Helper(0, makeIndexMap(binaryStrings))
    return binaryToDecimal(co2ScrubberString)
}

def calcLifeSupport(binaryStrings: List[List[Char]]): Int = calcC02Rate(binaryStrings) * calcOxygenRate(binaryStrings)

def calcGammaAndEpsilonBinary(binaryStrings: List[List[Char]]): (String, String) = {
    val freqBits = binaryStrings.transpose
    val gammaRateBits = freqBits.map(bits => if (bits.count(_ == '1') >= bits.length - bits.count(_ == '1')) '1' else '0')
    val epsilonRate = gammaRateBits.map(x => if (x == '1') '0' else '1').mkString 
    val gammaRate = gammaRateBits.mkString
    return (gammaRate, epsilonRate)
}

def calcGammaAndEpsilon(binaryStrings: List[List[Char]]): (Int, Int) = {
    val (gammaRateBinString, epsilonRateBinString) = calcGammaAndEpsilonBinary(binaryStrings)
    val epsilonRate = binaryToDecimal(epsilonRateBinString)
    val gammaRate = binaryToDecimal(gammaRateBinString)
    return (gammaRate, epsilonRate)
}

def calcPowerConsumption: List[List[Char]] => Int = calcGammaAndEpsilon andThen ((x, y) => x * y)
 
@main def main = {
    val filename = "input.txt"
    val binaryStrings: List[List[Char]] = Source.fromFile(filename).getLines.toList.map(_.toList)
    println(calcPowerConsumption1(binaryStrings))
    println(calcLifeSupport(binaryStrings))
}