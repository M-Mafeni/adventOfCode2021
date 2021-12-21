import scala.math.pow
import scala.io.Source
/*
Logic
header = packet version + packet type ID
packet version = first 3 bits
packet type ID = next 3 bits
If type ID = 4  then lit value
    process lit Value
else operator 
    bit after header = length type ID
    if length type ID = 0 then
        next 15 bits = represent total length in bits of sub packets
    else
        next 11 bits = number of sub-packets immediately contained by packet
*/
type Hex = String
type Bin = String
type Packet = LiteralPacket | OperatorPacket
case class LiteralPacket (
    version: Int,
    value: BigInt
)
enum LengthTypeID :
    case FifteenBit, ElevenBit

case class OperatorPacket (
    id: Int,
    version: Int,
    mode: LengthTypeID,
    subPackets: List[Packet]
)

def binToBigInt(bin: Bin): BigInt = {
    val exp = bin.length - 1
    def reducer(acc: (Int, BigInt), bit: Char) = {
        val (exp, value) = acc
        bit match {
            case '0' => (exp - 1, value)
            case '1' => (exp - 1, value + (BigInt(2) pow exp))
        }
    }
    bin.toList.foldLeft((exp, BigInt(0)))(reducer)._2
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

case class LitValState (
    bits: Bin,
    isLast: Boolean,
    bitsProcessed: Int
)

case class PacketState (
    bits: Bin,
    bitsProcessed: Int
)

def parseSubBinState(state: LitValState): (Bin, LitValState) = {
    val (bitsNeeded, remaining) = state.bits.splitAt(5)
    (bitsNeeded, LitValState(remaining, bitsNeeded.head == '0', state.bitsProcessed + bitsNeeded.length))
}
def getSubBinsHelperState(state: LitValState, acc: List[Bin]): (List[Bin], LitValState) = {
    if (state.isLast) {
        (acc.reverse, state)
    } else {
        val (bin, nextState) = parseSubBinState(state)
        getSubBinsHelperState(nextState, bin :: acc)
    }
}

def getSubBinsState(state: LitValState): (List[Bin], LitValState) = getSubBinsHelperState(state, List())

def runSubPacketsFifteenBitState(state: PacketState, maxLenBits: Int, packets: List[Packet]): (List[Packet], PacketState) = {
    if (state.bitsProcessed == maxLenBits) {
        (packets.reverse, state)
    } else {
        val (packet, nextState) = parsePacketHelper(state)
        runSubPacketsFifteenBitState(nextState, maxLenBits, packet :: packets)
    }
}

def runSubPacketsElevenBitState(state: PacketState, maxPackets: Int, packets: List[Packet]): (List[Packet], PacketState) = {
    if (packets.length == maxPackets) {
        (packets.reverse, state)
    } else {
        val (packet, nextState) = parsePacketHelper(state)
        runSubPacketsElevenBitState(nextState, maxPackets, packet :: packets)
    }
}

def parsePacketHelper(packetState: PacketState): (Packet, PacketState) = {
    val bits = packetState.bits
    val (header, remainingBits) = bits.splitAt(6)
    val (packetBits, versionBits) = header.splitAt(3)
    val version = binToBigInt(packetBits).toInt
    val packetId = binToBigInt(versionBits).toInt
    packetId match {
        case 4 => {
            val (bins, litValBinState) = getSubBinsState(LitValState(remainingBits, false, 0))
            val value = binToBigInt(bins.map(_.tail).mkString)
            val bitsProcessed = packetState.bitsProcessed + litValBinState.bitsProcessed + 6
            (LiteralPacket(version, value), PacketState(litValBinState.bits, bitsProcessed))
        }
        case n => {
            val lengthTypeBit = remainingBits.head
            val lengthTypeMode = if lengthTypeBit == '1' then LengthTypeID.ElevenBit else LengthTypeID.FifteenBit
            lengthTypeMode match {
                case LengthTypeID.FifteenBit => {
                    val (bitsNeededForParseVal, newRemainderForBits) = remainingBits.tail.splitAt(15)
                    val parseVal = binToBigInt(bitsNeededForParseVal).toInt
                    val (subPackets, subPacketsState) = runSubPacketsFifteenBitState(PacketState(newRemainderForBits, 0), parseVal, List())
                    val bitsProcessed = packetState.bitsProcessed + subPacketsState.bitsProcessed + 22
                    (OperatorPacket(packetId, version, lengthTypeMode, subPackets), PacketState(subPacketsState.bits, bitsProcessed))
                }
                case LengthTypeID.ElevenBit => {
                    val (bitsNeededForParseVal, newRemainderForBits) = remainingBits.tail.splitAt(11)
                    val parseVal = binToBigInt(bitsNeededForParseVal).toInt
                    val (subPackets, subPacketsState) = runSubPacketsElevenBitState(PacketState(newRemainderForBits, 0), parseVal, List())
                    val bitsProcessed = packetState.bitsProcessed + subPacketsState.bitsProcessed + 18
                    (OperatorPacket(packetId, version, lengthTypeMode, subPackets), PacketState(subPacketsState.bits, bitsProcessed))
                }
            }
        }
    }
}

def versionSum(packet: Packet): Int = packet match {
    case LiteralPacket(version, _) => version
    case OperatorPacket(_, version, _, subPackets) => version + subPackets.map(versionSum).sum
}
def parsePacket(hex: Hex): Packet = parsePacketHelper(PacketState(hexToBin(hex), 0))._1

def evalPacket(packet: Packet): BigInt = packet match {
    case LiteralPacket(_, value) => value
    case OperatorPacket(id, _, _, subPackets) => {
        val values = subPackets.map(evalPacket)
        id match {
            case 0 => values.sum
            case 1 => values.product
            case 2 => values.min
            case 3 => values.max
            case 5 => {
                val List(a, b) = values
                if a > b then BigInt(1) else BigInt(0)
            }
            case 6 => {
                val List(a, b) = values
                if a < b then BigInt(1) else BigInt(0)
            }
            case 7 => {
                val List(a, b) = values
                if a == b then BigInt(1) else BigInt(0)
            }

        }
    } 
}

def evalPacketHex = parsePacket andThen evalPacket

def runExamples = {
    println(parsePacket("D2FE28"))
    println(parsePacket("38006F45291200"))
    println(parsePacket("EE00D40C823060"))
    println(versionSumHex("8A004A801A8002F478"))
    println(versionSumHex("620080001611562C8802118E34"))
    println(versionSumHex("C0015000016115A2E0802F182340"))
    println(versionSumHex("A0016C880162017C3686B18A3D4780"))
}

def runExamples2 = {
    println(evalPacketHex("C200B40A82"))
    println(evalPacketHex("04005AC33890"))
    println(evalPacketHex("880086C3E88112"))
    println(evalPacketHex("CE00C43D881120"))
    println(evalPacketHex("D8005AC2A8F0"))
    println(evalPacketHex("F600BC2D8F"))
    println(evalPacketHex("9C005AC2F8F0"))
    println(evalPacketHex("9C0141080250320F1802104A08"))
}

def versionSumHex = parsePacket andThen versionSum
@main def main = {
    // CUrr answer = 9964787350
    val hex = Source.fromFile("input.txt").mkString
    val packet = parsePacket(hex)
    println(versionSum(packet))
    println(evalPacket(packet))
}
