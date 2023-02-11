package P01_Elementary

object P08PrimeNumbers {
  def P8PrimeNumbers(): Unit = {
    try {
      var num = 2
      var tmp = 2
      var break = false
      while (true) {
        tmp = 2
        break = false
        while (tmp <= math.sqrt(num).toInt && !break) {
          if (num % tmp == 0) {
            break = true
          }
          tmp += 1
        }
        if (!break) println(num)
        num = Math.addExact(num, 1)
      }
    } catch {
      case ex: java.lang.ArithmeticException => println("Integer's bound reached")
    }
  }
}
