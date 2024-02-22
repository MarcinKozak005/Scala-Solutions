import scala.annotation.tailrec

class P20_PascalsTriangle {

}

object PascalsTriangle {
  def rows(numberOfRows: Integer): List[List[Int]] = rowsHelper(numberOfRows, List(), 0)

  @tailrec
  def rowsHelper(numberOfRows: Integer, result: List[List[Int]], currentRow: Int): List[List[Int]] = currentRow match {
    case currRow if currRow >= numberOfRows => result
    case 0 => rowsHelper(numberOfRows, List(List(1)), currentRow + 1)
    case _ => rowsHelper(numberOfRows, generateNextRow(result), currentRow + 1)
  }

  def generateNextRow(currentTriangle: List[List[Int]]): List[List[Int]] = {
    val lastRow = currentTriangle.last
    val r1 = lastRow :+ 0
    val r2 = 0 +: lastRow
    currentTriangle :+ r1.lazyZip(r2).map(_ + _)
  }

}