import scala.io.Source

def countIncreases(depths: List[Int], isSlidingWindow: Boolean): Int = {
    /*
        go through list if curr value is bigger than prev value increase
    */
    def countIncreaseHelper(values: List[Int], total: Int): Int = {
        if (values.tail.isEmpty) {
            return total
        }
        val curr = values.head
        val next = values.tail.head
        if (next > curr) {
            return countIncreaseHelper(values.tail, total + 1)
        } else {
            return countIncreaseHelper(values.tail, total)
        }
    }

     def countIncreaseHelperWindow(values: List[Int], total: Int): Int = {
        if (values.tail.length < 3) {
            return total
        }
        val currSlidingWindow = values.take(3)
        val nextSlidingWindow = values.tail.take(3)
        if (nextSlidingWindow.sum > currSlidingWindow.sum) {
            return countIncreaseHelperWindow(values.tail, total + 1)
        } else {
            return countIncreaseHelperWindow(values.tail, total)
        }
    }
    return if isSlidingWindow then countIncreaseHelperWindow(depths, 0) else countIncreaseHelper(depths, 0)
}

object Solution {
    
    def main() = {
        val filename = "input.txt"
        // Read depths from input.txt
        val bufferedSource = Source.fromFile(filename)
        val depths: List[Int] = bufferedSource.getLines().toList.map(_.toInt)
        println(countIncreases(depths, false))
        println(countIncreases(depths, true))
        bufferedSource.close
    }
}