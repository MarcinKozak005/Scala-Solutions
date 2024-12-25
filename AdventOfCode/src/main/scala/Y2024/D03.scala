package Y2024

import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object D03 {

  private val MulRegex = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

  private def multiplyMulNumbers(m: Regex.Match): Long = m.group(1).toInt * m.group(2).toInt

  private def processFile(processFunction: Iterator[String] => Long): Unit = {
    Using(Source.fromFile("src/main/resources/2024/D03.txt")) { file =>
      processFunction(file.getLines())
    }.fold(
      e => println(e.getMessage),
      result => println(result),
    )
  }

  def P01(): Unit =
    processFile(_.map { line =>
      MulRegex.findAllMatchIn(line).map(multiplyMulNumbers).sum
    }.sum)

  def P02(): Unit = {
    val dontDoRegex = "don't\\(\\).*?(do\\(\\)|$)".r
    processFile { lines =>
      MulRegex.findAllMatchIn(dontDoRegex.replaceAllIn(lines.mkString(""), "")).map(multiplyMulNumbers).sum
    }
  }


  def main(args: Array[String]): Unit = {
    P01()
    P02()
  }
}
