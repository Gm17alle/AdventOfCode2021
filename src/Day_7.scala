import Utils.readCommaSeperatedIntsToList

object Day_7 {

  def problem1(crabLocs: List[Int]): Int = {
    val moveCost = Array.ofDim[Int](crabLocs.max)

    for (i <- 0 to moveCost.length - 1) yield moveCost(i) = crabLocs.map(crabLoc => getFuelCost((crabLoc - i).abs)).sum
    moveCost.min
  }

  def getFuelCost(i: Int): Int = {
    // i // part 1
    (i * (i+1)) / 2 // Part 2
  }



  def main(args: Array[String]): Unit = { //94990412 is too low
    print(problem1(readCommaSeperatedIntsToList("src/resources/crabs.txt")))
//    print(problem1(List(16,1,2,0,4,2,7,1,2,14)))
  }
}
