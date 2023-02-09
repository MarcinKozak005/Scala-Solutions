package P01_Elementary

import java.time.Year

object P10Next10LeapYears {
  def P10Next10LeapYears(): Unit = {
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
}
