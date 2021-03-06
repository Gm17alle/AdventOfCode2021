import scala.io.Source
import scala.util.Using

case class letterNumbers(numberDef: List[String], output: List[String])

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
    Using(Source.fromFile(filename)) { source => source.getLines().toList }.get
  }

  def readBingoBoards(filename: String): (List[Int], List[Array[Array[Int]]]) = {
    Using(Source.fromFile(filename)) { source =>
      val lines = source.getLines()
      val calledNums = lines.next().split(",").map(_.toInt).toList

      val bingoBoards = lines.toList.filter(_.trim != "").flatMap(_.split(" ").filter(_.trim != "").map(_.toInt)).toArray.grouped(5).toArray.grouped(5).toList

      (calledNums, bingoBoards)
    }.get
  }

  def readInto2dArrays(rows: List[String]): List[Array[Array[Int]]] = {
    rows.filter(_.trim != "").flatMap(_.split(" ").filter(_.trim != "").map(_.toInt)).toArray.grouped(5).toArray.grouped(5).toList
  }

  def readCommaSeperatedIntsToList(filename: String): List[Int] = {
    Using(Source.fromFile(filename)) {
      _.getLines().next().split(",").map(_.toInt).toList
    }
  }.get

  def readWeirdLetters(filename: String): List[letterNumbers] = {
    Using(Source.fromFile(filename)) {
      _.getLines().toList.map(s =>
        letterNumbers(s.split(" \\| ")(0).split(" ").map(_.trim.sorted).toList, s.split(" \\| ")(1).split(" ").map(_.trim.sorted).toList))

    }.get
  }

  def readDigits(filename: String): List[List[Int]] = {
    Using(Source.fromFile(filename)) {
      _.getLines().map(_.map(_.toInt - '0').toList).toList
    }
  }.get


}
