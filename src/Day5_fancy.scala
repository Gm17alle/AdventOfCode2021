//found from https://www.reddit.com/r/adventofcode/comments/r9824c/2021_day_5_solutions/hnctzl5/?context=3

import scala.io.Source
import scala.util.Using

object Day5_fancy {

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {
    def straight: Boolean = start.x == end.x || start.y == end.y
    def allPoints: List[Point] = {
      val (dx, dy) = (end.x compare start.x, end.y compare start.y)
      val steps = (start.x - end.x).abs.max((start.y-end.y).abs)
      (for(i <- 0 to steps) yield Point(start.x + i * dx, start.y + i * dy)).toList
    }
  }

  def readFileIntoSetOfLines(filename: String): List[Line] = {
    Using(Source.fromFile(filename)) {
      _.getLines().toList.map(_.trim.split(" -> |,") match {
        case Array(x1: String, y1: String, x2: String, y2: String) => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      })
    }.get
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileIntoSetOfLines("src/resources/lines.txt")
    println(s"answer " + lines.flatMap(_.allPoints).groupMapReduce(identity)(_ => 1)(_ + _).count(_._2 > 1))
  }

}


//.map(_.replace("->", ",").split(",").toList.map(_.trim.toInt) match {
//  case List(a, b, c, d) => ((a, b), (c, d))
//}).toList