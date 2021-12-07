import Utils.readCommaSeperatedIntsToList

import scala.annotation.tailrec

object Day_6 {



  def problem1(fish: List[Int]): Int = {
    @tailrec
    def helper(fish: List[Int], day: Int, maxDays: Int): Int = {
      if(day == maxDays) fish.length
      else {
        val newFish = fish.count(_==0)
        helper(fish.map(ageFish) ++ List.fill(newFish)(8), day + 1, maxDays)
      }
    }
    helper(fish, 0, 80)
  }

  def ageFish(f: Int): Int = {
    if(f == 0) 6
    else f-1
  }

  def problem2(fish: List[Int]): BigInt = {
    val fishByDay = fish.groupMapReduce(identity)(_ => 1)(_ + _)
    val fishArray = Array.fill[BigInt](9)(0)
    fishByDay.foreach(p => fishArray(p._1-1) = p._2)
    for (i <- 1 to 255) yield (moveFish(fishArray))
    fishArray.sum
  }

  def moveFish(fishArray: Array[BigInt]): Unit = {
    val slidingSlice = fishArray.slice(1,9)
    val new6and8 = fishArray(0)
    slidingSlice.copyToArray(fishArray)
    fishArray(6) += new6and8;
    fishArray(8) = new6and8
    new6and8
    ()
  }

  def main(args: Array[String]): Unit = { // > 45122333680048
    print(problem2(readCommaSeperatedIntsToList("src/resources/lanternfish.txt")))
//    print(problem2((1 to 5).toList))
  }
}
