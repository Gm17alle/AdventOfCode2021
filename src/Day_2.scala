import Utils.readFileToStringIntTuple

import scala.annotation.tailrec

object Day_2 {

  def problem1(filename: String): Int = {
    val instructions = readFileToStringIntTuple(filename)
    val forward = instructions.collect {
      case ("forward", x) => x
      case (_, _) => 0
    }.sum
    println(s"forward: $forward")
    val depth = instructions.collect {
      case ("up", x) => -x
      case ("down", x) => x
      case (_, _) => 0
    }.sum

    forward * depth
  }

  def problem2(filename: String): Int = {
    val instructions = readFileToStringIntTuple(filename)
    val coords = getCoordinates(instructions)
    coords._2 * coords._3

  }

  def getCoordinates(instructions: List[(String, Int)]): (Int, Int, Int) = {
    @tailrec
    def helper(instructions: List[(String, Int)], aim: Int, position: Int, depth: Int): (Int, Int, Int) = {
      if(instructions.isEmpty) return (aim, position, depth)
      else if(instructions.head._1 == "down") helper(instructions.tail, aim + instructions.head._2, position, depth)
      else if(instructions.head._1 == "up") helper(instructions.tail, aim - instructions.head._2, position, depth)
      else helper(instructions.tail, aim, position + instructions.head._2, depth + instructions.head._2 * aim)
    }
    helper(instructions, 0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    print(problem2("src/resources/directions.txt"))
  }
}
