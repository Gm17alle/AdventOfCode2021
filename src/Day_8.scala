import Utils.readWeirdLetters

import scala.annotation.switch

object Day_8 {


  def problem1(input: List[letterNumbers]): Int = {
    input.map(_.output.count(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)).sum
  }

  def problem2(input: List[letterNumbers]): Int = {
    input.map(ln => toOutputSum(ln.numberDef, ln.output)).sum
  }

  def toOutputSum(inputs: List[String], outputs: List[String]): Int = {

    // Figure out which letter corresponds to which segment using the numbers we know
    // Use those segments to figure out the numbers we don't (i.e. 0 is 8 minus middle segment)
    // map results to digits, then :shipit:

    val eight = inputs.filter(_.length == 7).head
    val one = inputs.filter(_.length == 2).head
    val seven = inputs.filter(_.length == 3).head
    val four = inputs.filter(_.length == 4).head
    val three = inputs.filter(_.length == 5).filter(containsSegments(one, _)).head // Needs contains thee chars, not just contains substring

    val toptop = seven.filterNot(one.contains(_)).head
    val bottomleft = eight.filterNot(four.contains(_)).filterNot(three.contains(_)).head
    val nine = eight.filterNot(_ == bottomleft)
    val topleft = eight.filter(c => !three.contains(c) && nine.contains(c))

    val five = inputs.filter(num => num.length == 5 && containsSegments(nine diff seven, num)).head
    val six = (five + bottomleft).sorted
    val knownNums = List(one, three, four, five, six, seven, eight, nine)
    val two = (inputs diff knownNums).filter(_.length == 5).head
    val zero = (inputs diff knownNums).filterNot(_ == two).head

    def stringToNums(num: String): String = (num: @switch) match {
      case `zero` => "0"
      case `one` => "1"
      case `two` => "2"
      case `three` => "3"
      case `four` => "4"
      case `five` => "5"
      case `six` => "6"
      case `seven` => "7"
      case `eight` => "8"
      case `nine` => "9"
      case _ => throw new Exception("something went bad")
    }

    val x = outputs.map(stringToNums).mkString.toInt
    x
  }

  def containsSegments(small: String, big: String): Boolean = {
    small.forall(big.contains(_))
  }


  def main(args: Array[String]): Unit = {
    //    print(problem1(readWeirdLetters("src/resources/letterNums.txt")))
    print(problem2(readWeirdLetters("src/resources/letterNums.txt")))
  }
}


