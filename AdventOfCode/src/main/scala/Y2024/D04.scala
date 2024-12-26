package Y2024

import scala.io.Source
import scala.util.{Try, Using}

object D04 {

  def P01(): Unit = processFile(checkXMAS)

  def P02(): Unit = processFile(checkX_MASCross)

  private def checkXmasCandidate(potentialWord: Try[String]): Int = {
    potentialWord.map(str => (str == "XMAS").compare(false)).getOrElse(0)
  }

  private def checkCrossDiagonal(diagonal: Try[String]): Boolean = {
    diagonal.map(str => str == "MAS" || str == "SAM").getOrElse(false)
  }

  private def checkCrossDescendingDiagonal(x: Int, y: Int, array: Array[String]): Boolean = {
    checkCrossDiagonal(Try(s"${array(y - 1)(x - 1)}${array(y)(x)}${array(y + 1)(x + 1)}"))
  }

  private def checkCrossAscendingDiagonal(x: Int, y: Int, array: Array[String]): Boolean = {
    checkCrossDiagonal(Try(s"${array(y - 1)(x + 1)}${array(y)(x)}${array(y + 1)(x - 1)}"))
  }

  private def checkX_MASCross(x: Int, y: Int, array: Array[String]): Int =
    if (checkCrossDescendingDiagonal(x, y, array) && checkCrossAscendingDiagonal(x, y, array)) 1 else 0

  private def checkRight(x: Int, y: Int, array: Array[String]): Int = checkXmasCandidate(Try(array(y).substring(x, x + 4)))

  private def checkLeft(x: Int, y: Int, array: Array[String]): Int = checkXmasCandidate(Try(array(y).substring(x - 3, x + 1).reverse))

  private def checkUp(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y)(x)}${array(y - 1)(x)}${array(y - 2)(x)}${array(y - 3)(x)}"))

  private def checkDown(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y)(x)}${array(y + 1)(x)}${array(y + 2)(x)}${array(y + 3)(x)}"))

  private def checkRightDown(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y + 0)(x + 0)}${array(y + 1)(x + 1)}${array(y + 2)(x + 2)}${array(y + 3)(x + 3)}"))

  private def checkRightUp(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y - 0)(x + 0)}${array(y - 1)(x + 1)}${array(y - 2)(x + 2)}${array(y - 3)(x + 3)}"))

  private def checkLeftUp(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y - 0)(x - 0)}${array(y - 1)(x - 1)}${array(y - 2)(x - 2)}${array(y - 3)(x - 3)}"))

  private def checkLeftDown(x: Int, y: Int, array: Array[String]): Int =
    checkXmasCandidate(Try(s"${array(y + 0)(x - 0)}${array(y + 1)(x - 1)}${array(y + 2)(x - 2)}${array(y + 3)(x - 3)}"))

  private def checkXMAS(x: Int, y: Int, array: Array[String]): Int =
    checkUp(x, y, array) +
      checkRightUp(x, y, array) +
      checkRight(x, y, array) +
      checkRightDown(x, y, array) +
      checkDown(x, y, array) +
      checkLeftDown(x, y, array) +
      checkLeft(x, y, array) +
      checkLeftUp(x, y, array)

  def processFile(processFunction: (Int, Int, Array[String]) => Int): Unit =
    Using(Source.fromFile("src/main/resources/2024/D04.txt")) { file =>
      val lines = file.getLines().toArray
      (0 until lines(0).length).map { x =>
        (lines.indices map { y =>
          processFunction(x, y, lines)
        }).sum
      }.sum
    }.fold(
      e => println(e.getMessage),
      result => println(result),
    )

  def main(args: Array[String]): Unit = {
    P01()
    P02()
  }


}
