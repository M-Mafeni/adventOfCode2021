package packet
import scala.math.pow

type Hex = String
type Bin = String
type Packet = LiteralPacket | OperatorPacket
case class LiteralPacket (
    version: Int,
    value: Int
)
enum LengthTypeID :
    case FifteenBit, ElevenBit

case class OperatorPacket (
    id: Int,
    version: Int,
    mode: LengthTypeID,
    subPackets: List[Packet]
)

def binToInt(bin: Bin): Int = {
    val exp = bin.length - 1
    def reducer(acc: (Int, Int), bit: Char) = {
        val (exp, value) = acc
        bit match {
            case '0' => (exp - 1, value)
            case '1' => (exp - 1, value + pow(2, exp).toInt)
        }
    }
    bin.toList.foldLeft((exp, 0))(reducer)._2
}
def hexToBin(hex: Hex): Bin = {
    def hexBitToBinary(c: Char): Bin = c match {
        case '0' => "0000"
        case '1' => "0001"
        case '2' => "0010"
        case '3' => "0011"
        case '4' => "0100"
        case '5' => "0101"
        case '6' => "0110"
        case '7' => "0111"
        case '8' => "1000"
        case '9' => "1001"
        case 'A' => "1010"
        case 'B' => "1011"
        case 'C' => "1100"
        case 'D' => "1101"
        case 'E' => "1110"
        case 'F' => "1111"
    }
    hex.toList.map(hexBitToBinary).mkString
}
