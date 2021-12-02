import Utils.readFileToArrayInts

import scala.annotation.tailrec

object Day1 {

  def problem1(): Int = {
    val readings = readFileToArrayInts("src/resources/radar.txt")
    countTripleIncreases(readings)
  }

  def countIncreases(list: List[Int]): Int = {
    @tailrec
    def helper(list: List[Int], count: Int): Int = {
      if(list.size < 2) count
      else if(list.head < list(1)) helper(list.tail, count+1)
      else helper(list.tail, count)
    }
    helper(list, 0)
  }

  def countTripleIncreases(list: List[Int]): Int = {
    @tailrec
    def helper(list: List[Int], count: Int): Int = {
      if(list.size < 4) count
      else if(list.head + list(1) + list(2) < list(1) + list(2) + list(3)) helper(list.tail, count+1)
      else helper(list.tail, count)
    }
    helper(list, 0)
  }

  //    if(list.size < 2) 0
  //    if(list.head < list(1)) countIncreases(list.tail) + 1
  //    else countIncreases(list.tail)

  def main(args: Array[String]): Unit = {
    print(problem1())
  }
}



