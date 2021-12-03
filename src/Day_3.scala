import Utils.readFileToArrayString

import scala.annotation.tailrec

object Day_3 {

  def problem1(filename: String): Int = {
    val binaryNumbers = readFileToArrayString(filename)
    val gamma = getPopularChars(binaryNumbers)
    val epsilon = gamma.collect {
      case '1' => "0"
      case '0' => "1"
    }.mkString

    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
  }

  def getPopularChar(nums: List[String], i: Int): Char = {
    nums
      .collect(_(i))
      .sortWith(_ > _)(nums.length / 2) //hacky but w/e
  }

  def getPopularChars(nums: List[String]): String = {
    @tailrec
    def helper(nums: List[String], s: String, i: Int): String = {
      if(i >= nums.head.length) s
      else helper(nums, s + getPopularChar(nums, i), i+1)
    }
    helper(nums, "", 0)
  }


  def main(args: Array[String]): Unit = {
    print(problem1("src/resources/binary.txt"))
  }

}
