import scala.io.Source
import scala.util.Using

object Utils {

  def readFileToArrayInts(filename: String): List[Int] = {
    Using(Source.fromFile(filename)) { source => source.getLines().map(_.toInt).toList }.get
  }

  def readFileToStringIntTuple(filename: String): List[(String, Int)] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().collect(_.split(" ") match {
        case Array(direction, value) => (direction, value.toInt)
      }).toList
    }.get
  }

  def readFileToArrayString(filename: String): List[String] = {
    Using(Source.fromFile(filename)) {source => source.getLines().toList}.get
  }
}
