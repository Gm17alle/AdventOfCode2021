import Utils.readBingoBoards

import scala.annotation.tailrec

object Day_4 {

  def problem1(tuple: (List[Int], List[Array[Array[Int]]])): Int = {
    val (calledNums, boards) = tuple
    calculateFirstSolvedBingoValue(calledNums, boards, -1)
  }

  @tailrec
  def calculateFirstSolvedBingoValue(calledNums: List[Int], boards: List[Array[Array[Int]]], lastCalledNum: Int): Int = {
    if(boards.exists(hasBingo)) {
      sumUnmarked(boards.filter(hasBingo).head) * lastCalledNum
    } else {
      calculateFirstSolvedBingoValue(calledNums.tail, boards.map(markValue(_, calledNums.head)), calledNums.head)
    }
  }

  def markValue(board: Array[Array[Int]], value: Int): Array[Array[Int]] = {
    board.map(_.map(x => if(x == value) -1 else x))
  }

  def hasBingo(board: Array[Array[Int]]): Boolean = {
    board.exists(isBingoRowCol) || board.transpose.exists(isBingoRowCol)

  }

  def isBingoRowCol(rowCol: Array[Int]): Boolean = {
    rowCol.sum == -5
  }

  def sumUnmarked(board: Array[Array[Int]]): Int = {
    board.flatten.filter(_ != -1).sum
  }

  def problem2(tuple: (List[Int], List[Array[Array[Int]]])): Int = {
    val (calledNums, boards) = tuple
    @tailrec
    def helper(calledNums: List[Int], boards: List[Array[Array[Int]]], lastCalledNum: Int): Int = {
      if(boards.count(hasBingo) == 99) {
        calculateFirstSolvedBingoValue(calledNums, boards.filterNot(hasBingo), -2)
      } else {
        helper(calledNums.tail, boards.map(markValue(_, calledNums.head)), calledNums.head)
      }
    }
    helper(calledNums, boards, -1)
  }

  def main(args: Array[String]): Unit = {
    //Reading the values in here now because it was slightly less annoying to test - consistency is overrated :)
//    println(problem1(readBingoBoards("src/resources/bingo.txt")))
    println(problem2(readBingoBoards("src/resources/bingo.txt")))
  }
}
