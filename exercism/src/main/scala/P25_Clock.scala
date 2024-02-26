class P25_Clock {

}


case class Clock(hours: Int, minutes: Int) {
  def +(that: Clock): Clock =
    Clock.apply(this.hours + that.hours, this.minutes + that.minutes)

  def -(that: Clock): Clock =
    Clock.apply(this.hours - that.hours, this.minutes - that.minutes)
}


private object Clock {
  def apply(hours: Int, minutes: Int): Clock = {
    val minutesReduced = minutes % 60
    val minutesResult = (minutesReduced + 60) % 60
    val hoursModifier = (minutes / 60) + (if (minutesReduced < 0) -1 else 0)
    val hoursReduced = (hours + hoursModifier) % 24
    val hoursResult = (hoursReduced + 24) % 24
    new Clock(hoursResult, minutesResult)
  }

  def apply(minutes: Int): Clock = {
    Clock.apply(0, minutes)
  }
}