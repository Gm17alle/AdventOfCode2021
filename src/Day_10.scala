import Utils.readFileToArrayString

object Day_10 {

  def problem1(lines: List[String]): Int = {

    lines.map(getScore).sum
  }


  val openChars = List('<', '{', '[', '(')
  val closeChars = List('>', '}', ']', ')')

  val values = List(25137, 1197, 57, 3)

  val autoCompleteScores = List(4, 3, 2, 1)
  def getScore(line: String): Int = {
    def helper(line: String, stack: List[Char]): Int = {
      if(line.isEmpty) 0
      else if(openChars.contains(line.head)) helper(line.tail, line.head :: stack)
      else if(closeChars.indexOf(line.head) == openChars.indexOf(stack.head)) helper(line.tail, stack.tail)
      else values(closeChars.indexOf(line.head))
    }
    helper(line, List())
  }

  def isNotCorrupt(line: String): Boolean = {
    def helper(line: String, stack: List[Char]): Boolean = {
      if(line.isEmpty) true
      else if(openChars.contains(line.head)) helper(line.tail, line.head :: stack)
      else if(closeChars.indexOf(line.head) == openChars.indexOf(stack.head)) helper(line.tail, stack.tail)
      else false
    }
    helper(line, List())
  }

  def getClosureStackScore(line: String): BigInt = {
    def helper(line: String, stack: List[Char]): List[Char] = {
      if(line.isEmpty) stack
      else if(openChars.contains(line.head)) helper(line.tail, line.head :: stack)
      else if(closeChars.indexOf(line.head) == openChars.indexOf(stack.head)) helper(line.tail, stack.tail)
      else stack
    }
    helper(line, List()).foldLeft[BigInt](0)((total, c) => total * 5 + autoCompleteScores(openChars.indexOf(c)))
  }

  def problem2(lines: List[String]): BigInt = {
    val scores = lines.filter(isNotCorrupt).map(getClosureStackScore).sorted
    scores(scores.length/2)
  }


  def main(args: Array[String]): Unit = { // > 162764112
    println(problem2(readFileToArrayString("src/resources/routes.txt")))
  }
}
