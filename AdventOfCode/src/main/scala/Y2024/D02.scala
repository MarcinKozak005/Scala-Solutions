package Y2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object D02 {

  def P01(): Unit = processFile(validateReports01)

  def P02(): Unit = processFile(validateReports02)

  def processFile(validationFunction: (List[Int], Option[Boolean]) => Boolean): Unit =
    Using(Source.fromFile("src/main/resources/2024/D02.txt")) { file =>
      file.getLines().count { line =>
        val values = line.split("\\s+").map(_.toInt).toList
        validationFunction(values, None)
      }
    }.fold(
      e => println(e.getMessage),
      result => println(result),
    )

  private def isInRange(diff: Int): Boolean = (-3 to 3 contains diff) && diff != 0

  private def isStrictlyMonotonic(diff: Int, isRaising: Option[Boolean]): Boolean =
    isRaising.forall(_ == (diff > 0))

  @tailrec
  private def validateReports01(reports: List[Int], isRaising: Option[Boolean] = None): Boolean = reports match {
    case first :: second :: tail => second - first match {
      case diff if isStrictlyMonotonic(diff, isRaising) && isInRange(diff) =>
        validateReports01(second :: tail, isRaising.orElse(Option(diff > 0)))
      case _ => false
    }
    case _ => true
  }

  private def validateReports02(reports: List[Int], isRaising: Option[Boolean] = None): Boolean = {
    validateReports01(reports, isRaising) || validateWithoutSingleLevel(reports, 0)
  }

  @tailrec
  private def validateWithoutSingleLevel(reports: List[Int], levelToSkip: Int): Boolean = {
    if (levelToSkip >= reports.length) false
    else validateReports01(removeSingleLevel(reports, levelToSkip)) || validateWithoutSingleLevel(reports, levelToSkip + 1)
  }

  private def removeSingleLevel(reports: List[Int], whichToDrop: Int): List[Int] =
    reports.take(whichToDrop) ++ reports.drop(whichToDrop + 1)

  def main(args: Array[String]): Unit = {
    P01()
    P02()
  }

}
