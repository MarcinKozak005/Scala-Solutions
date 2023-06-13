package P01_Elementary

import java.time.Year
import scala.annotation.tailrec

object P10Next10LeapYears extends App {
  def solution1(): Unit = {
    var year = Year.now.getValue
    var counter = 0
    while (counter < 20) {
      if (year % 4 == 0 && year % 100 != 0 || year % 400 == 0) {
        println(year)
        counter += 1
      }
      year += 1
    }
  }

  def solution2(): Unit = {
    @tailrec
    def checkYear(year: Int = Year.now.getValue, counter: Int = 0): Unit = year match {
      case _ if counter == 20 =>
      case y if y % 4 == 0 && y % 100 != 0 || y % 400 == 0 =>
        println(year)
        checkYear(year + 1, counter + 1)
      case _ => checkYear(year + 1, counter)
    }

    checkYear()
  }

  solution1()
  solution2()
}
