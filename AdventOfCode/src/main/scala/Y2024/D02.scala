package Y2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object D02 {

  def P01(): Unit = processFile(isSafe)

  def processFile(validationFunction: List[Int] => Boolean): Unit =
    Using(Source.fromFile("src/main/resources/2024/D02.txt")) { file =>
      file.getLines().count { line =>
        val values = line.split("\\s+").map(_.toInt).toList
        validationFunction(values)
      }
    }.fold(
      e => println(e.getMessage),
      result => println(result),
    )

  private def isCorrectRange(diff: Int): Boolean = (-3 to 3 contains diff) && diff != 0

  private def isCorrectDiff(diff: Int, isRaising: Boolean): Boolean = isCorrectRange(diff) && (diff > 0) == isRaising

  private def isSafe(reports: List[Int]): Boolean = reports match {
    case first :: second :: tail => second - first match {
      case diff if isCorrectRange(diff) => validateReports(second :: tail, diff > 0)
      case _ => false
    }
    case _ => true
  }

  @tailrec
  private def validateReports(reports: List[Int], isRaising: Boolean): Boolean = reports match {
    case first :: second :: tail => second - first match {
      case diff if isCorrectDiff(diff, isRaising) => validateReports(second :: tail, isRaising)
      case _ => false
    }
    case _ => true
  }


  def main(args: Array[String]): Unit = {
    P01()
  }

}
