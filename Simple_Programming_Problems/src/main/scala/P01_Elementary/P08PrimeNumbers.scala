package P01_Elementary

import scala.annotation.tailrec

object P08PrimeNumbers extends App {
  def solution1(): Unit = {
    try {
      var num = 2
      var tmp = 2
      var break = false
      while (true) {
        tmp = 2
        break = false
        while (tmp <= math.sqrt(num).toInt && !break) {
          if (num % tmp == 0) break = true
          tmp += 1
        }
        if (!break) println(num)
        num = Math.addExact(num, 1)
      }
    } catch {
      case ex: java.lang.ArithmeticException => println(s"Integer's bound reached $ex")
    }
  }

  def solution2(): Unit = {
    @tailrec
    def checkSingleNumber(n: Int, currentValue: Int = 2): Boolean =
      if (currentValue > math.sqrt(n)) true
      else n % currentValue match {
        case 0 => false
        case _ => checkSingleNumber(n, currentValue + 1)
      }

    (1 to Int.MaxValue).filter(checkSingleNumber(_)).foreach(println)
  }

  solution1()
  solution2()
}
