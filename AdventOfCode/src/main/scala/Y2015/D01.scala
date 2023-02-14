package Y2015

import scala.annotation.tailrec
import scala.io.Source

object D01 {

  def P01(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D01.txt")
    val str = data.getLines.next()
    data.close()
    str.count(_.equals('(')) - str.count(_.equals(')'))
  }

  def P02(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D01.txt")
    val str = data.getLines.next()
    data.close()
    processChar(str, 0, 0)
  }

  @tailrec
  def processChar(str: String, position: Int, currentLevel: Int): Int =
    if (position >= str.length) -1
    else if (currentLevel == -1) position
    else processChar(
      str,
      position + 1,
      if (str.charAt(position) == '(') currentLevel + 1 else currentLevel - 1
    )

  def main(args: Array[String]): Unit = {
    println(P01())
    println(P02())
  }

}
