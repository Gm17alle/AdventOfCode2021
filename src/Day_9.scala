import Utils.readDigits

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day_9 {

  def problem1(nums: List[List[Int]]): Int = {
    (for {
      (x, i) <- nums.zipWithIndex
      (v, j) <- x.zipWithIndex
      if getAdjacentValues(nums, i, j).forall(_ > v)
    } yield v).map(_+1).sum

  }

  def getAdjacentValues(nums: List[List[Int]], i: Int, j: Int): List[Int] = {
    val array = Array.fill(4)(11)
    if(j < nums(0).length-1) {
      array(0) = nums(i)(j+1)
    }
    if(j > 0) {
      array(1) = nums(i)(j-1)
    }
    if(i < nums.length - 1) {
      array(2) = nums(i+1)(j)
    }
    if(i > 0) {
      array(3) = nums(i-1)(j)
    }
    array.toList
  }

  def problem2(nums: List[List[Int]]): Int = {
    val bottomPoints = getLowPoints(nums)
    val x = bottomPoints.map(p => getBasinSize(nums, p._1, p._2)).sortWith(_ > _)
      x.slice(0, 3).product
  }

  def getBasinSize(nums: List[List[Int]], iIndex: Int, jIndex: Int): Int = {
    @tailrec
    def helper(q: List[(Int, Int)], checked: List[(Int, Int)], result: Int): Int = {
      if(q.isEmpty)  result
      else {
        val i = q.head._1
        val j = q.head._2
        if(checked.contains(q.head)) helper(q.tail, checked, result)
        else if(i < 0 || i > nums.length - 1 || j < 0 || j > nums.head.length - 1) helper(q.tail, checked :+ q.head, result)
        else if(nums(i)(j) == 9) helper(q.tail, checked :+ q.head, result)
        else helper(q.tail ++ List((i-1, j), (i+1, j), (i, j-1), (i, j+1)), checked :+ q.head, result + 1)
      }
    }
    helper(List((iIndex, jIndex)), List(), 0)
  }

  def getLowPoints(nums: List[List[Int]]): List[(Int, Int)] = {
    for {
      (x, i) <- nums.zipWithIndex
      (v, j) <- x.zipWithIndex
      if getAdjacentValues(nums, i, j).forall(_ > v)
    } yield (i, j)
  }

  def main(args: Array[String]): Unit = { // < 9709
    println(problem2(readDigits("src/resources/smoke.txt")))
  }

}
