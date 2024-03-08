import Schedule.{First, Fourth, Last, Second, Teenth, Third}

import java.time.{DayOfWeek, LocalDate, YearMonth}

class P26_Meetup {

}

import Schedule.Schedule

case class Meetup(month: Int, year: Int) {

  def day(dayOfWeek: Int, schedule: Schedule): LocalDate = {
    val dayOfWeekInMonth = (1 to YearMonth.of(year, month).lengthOfMonth())
      .map(LocalDate.of(year, month, _))
      .filter(_.getDayOfWeek == DayOfWeek.of(dayOfWeek))
    schedule match {
      case First | Second | Third | Fourth => dayOfWeekInMonth(schedule.id)
      case Last => dayOfWeekInMonth.last
      case Teenth => dayOfWeekInMonth.filter(d => 13 <= d.getDayOfMonth && d.getDayOfMonth <= 19).head
    }
  }
}

object Schedule extends Enumeration {
  type Schedule = Value
  val First, Second, Third, Fourth, Last, Teenth = Value
}

object Meetup {
  val Mon: Int = DayOfWeek.MONDAY.getValue
  val Tue: Int = DayOfWeek.TUESDAY.getValue
  val Wed: Int = DayOfWeek.WEDNESDAY.getValue
  val Thu: Int = DayOfWeek.THURSDAY.getValue
  val Fri: Int = DayOfWeek.FRIDAY.getValue
  val Sat: Int = DayOfWeek.SATURDAY.getValue
  val Sun: Int = DayOfWeek.SUNDAY.getValue
}
