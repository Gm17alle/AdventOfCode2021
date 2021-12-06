
import Math.{abs, max, min}
import scala.io.Source
import scala.util.Using

object Day_5 {

  def problem1(pairs: List[((Int, Int), (Int, Int))]): Int = {
    val grid = Array.ofDim[Int](max(pairs.map(_._1._1).max, pairs.map(_._2._1).max) + 1,
      max(pairs.map(_._1._2).max, pairs.map(_._2._2).max) + 1)

    val horzLines = pairs.filter(p => p._1._1 == p._2._1)
    val vertLines = pairs.filter(p => p._1._2 == p._2._2)

    horzLines.foreach(p =>
      grid(p._1._1)
        .slice(min(p._1._2, p._2._2), max(p._1._2, p._2._2) + 1)
        .map(_ + 1)
        .copyToArray(grid(p._1._1), min(p._1._2, p._2._2))
    )

    val tiltedGrid = grid.transpose
    val temp = tiltedGrid.flatten.toList.count(_ > 0)
    vertLines.foreach(p =>
      tiltedGrid(p._1._2)
        .slice(min(p._1._1, p._2._1), max(p._1._1, p._2._1) + 1)
        .map(_ + 1)
        .copyToArray(tiltedGrid(p._1._2), min(p._1._1, p._2._1))
    )

    //problem 1 value
//    tiltedGrid.flatten.toList.count(_ > 1)

    val realGridShady = tiltedGrid.transpose

    val diagonals = pairs.filterNot(p => p._1._1 == p._2._1).filterNot(p => p._1._2 == p._2._2)
    diagonals.foreach(p => {
      val x_direction = p._2._1 compare p._1._1
      val y_direction = p._2._2 compare p._1._2
      val pointsToUpdate = (for(i <- 0 to abs(p._1._1 - p._2._1)) yield (p._1._1 + x_direction * i, p._1._2 + y_direction * i)).toList
      pointsToUpdate.foreach(p => {
        realGridShady(p._1)(p._2) += 1
      })
    })

    realGridShady.flatten.toList.count(_ > 1)
  }


  def vertOrHorz(coords: ((Int, Int), (Int, Int))): Boolean = {
    coords._1._1 == coords._2._1 || coords._1._2 == coords._2._2
  }

  def readFileIntoSetOfLines(filename: String): List[((Int, Int), (Int, Int))] = {
    Using(Source.fromFile(filename)) {
      _.getLines().toList.map(_.replace("->", ",").split(",").toList.map(_.trim.toInt) match {
        case List(a, b, c, d) => ((a, b), (c, d))
      }).toList
    }
  }.get

  def main(args: Array[String]): Unit = { // > 7638

    println(problem1(readFileIntoSetOfLines("src/resources/lines.txt")))
  }
}
