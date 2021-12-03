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
      .collect(_ (i))
      .sortWith(_ > _)(nums.length / 2) //hacky but w/e
  }

  def getPopularChars(nums: List[String]): String = {
    @tailrec
    def helper(nums: List[String], s: String, i: Int): String = {
      if (i >= nums.head.length) s
      else helper(nums, s + getPopularChar(nums, i), i + 1)
    }

    helper(nums, "", 0)
  }

  def determineOxygenDigit(nums: List[String], i: Int): Char = {
    val chars = nums.collect(_ (i))
    if (chars.count(_ == '0') > chars.count(_ == '1')) '0'
    else '1'
  }

  def determineCo2Digit(nums: List[String], i: Int): Char = {
    val chars = nums.collect(_ (i))
    if (chars.count(_ == '0') <= chars.count(_ == '1')) '0'
    else '1'
  }

  def problem2(filename: String): Int = {
    val binaryNumbers = readFileToArrayString(filename)
    val o2rating = getRating(binaryNumbers,  determineOxygenDigit)
    val co2rating = getRating(binaryNumbers,  determineCo2Digit)
    println(o2rating)
    println(co2rating)


    Integer.parseInt(o2rating, 2) * Integer.parseInt(co2rating, 2)
  }

  def getCharRemovingOthers(nums: List[String], i: Int, f: (List[String], Int) => Char): (Char, List[String]) = {
    val c = f(nums, i)
    val newNums = nums.filter(_(i) == c)
    (c, newNums)
  }

  def getRating(nums: List[String], f: (List[String], Int) => Char): String = {
    @tailrec
    def helper(nums: List[String], i: Int): String = {
      if (nums.length == 1) nums.head
      else {
        val charList = getCharRemovingOthers(nums, i, f)
        helper(charList._2, i + 1)
      }
    }

    helper(nums, 0)
  }


  def main(args: Array[String]): Unit = {
    print(problem2("src/resources/binary.txt"))
  }

}
