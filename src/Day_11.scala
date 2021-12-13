import Utils.readDigits

object Day_11 {

  case class Point(x: Int, y: Int)

  def problem1(array: Array[Array[Int]]): Int = {
    handleIt(array).sum
  }

  def problem2(array: Array[Array[Int]]): Int = {
    handleIt(array).indexOf(100) = 1
  }
  def handleIt(octopuses: Array[Array[Int]]): List[Int] = {
     (1 to 600).map(_ => handleDay(octopuses)).toList
  }

  def handleDay(octopuses: Array[Array[Int]]): Int = {
    val flashPoints = (for {
      (x, i) <- octopuses.zipWithIndex
      (v, j) <- x.zipWithIndex
    } yield {
      octopuses(i)(j) = v + 1
      if (v + 1 > 9) Point(i, j)
      else Point(-1, -1)
    }).filter(_.x > -1).toList

    val totalFlashes = handleFlashes(octopuses, flashPoints, List(), 0)

    for {
      (x, i) <- octopuses.zipWithIndex
      (v, j) <- x.zipWithIndex
      if v > 9
    } yield {
      octopuses(i)(j) = 0
    }

    totalFlashes
  }

  def handleFlashes(octopuses: Array[Array[Int]], toFlash: List[Point], flashed: List[Point], flashedCount: Int): Int = {
    if (toFlash.isEmpty) flashedCount
    else if (toFlash.head.x < 0 || toFlash.head.x > octopuses.length || toFlash.head.y < 0 || toFlash.head.y > octopuses.length || flashed.contains(toFlash.head)) handleFlashes(octopuses, toFlash.tail, flashed :+ toFlash.head, flashedCount)
    else {
      val newPoints = (for {
        x <- toFlash.head.x - 1 to toFlash.head.x + 1
        y <- toFlash.head.y - 1 to toFlash.head.y + 1
      } yield Point(x, y)).filterNot(_ == toFlash.head).filterNot(flashed contains _).filter(p => p.x > -1 && p.x < octopuses.length && p.y > -1 && p.y < octopuses.length)

      newPoints foreach { p => octopuses(p.x)(p.y) = octopuses(p.x)(p.y) + 1 }
      handleFlashes(octopuses, toFlash.tail ++ newPoints.filter(p => octopuses(p.x)(p.y) > 9), flashed :+ toFlash.head, flashedCount + 1)
    }
  }

  def main(args: Array[String]): Unit = { // > 399
    println(problem1(readDigits("src/resources/octopi.txt").toArray.map(_.toArray)))
    println(problem2(readDigits("src/resources/octopi.txt").toArray.map(_.toArray)))
  }
}
