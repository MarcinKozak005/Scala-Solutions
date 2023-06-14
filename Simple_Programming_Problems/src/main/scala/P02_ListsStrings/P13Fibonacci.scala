package P02_ListsStrings

import scala.annotation.tailrec

object P13Fibonacci {
  def P13ListFibonacci01(): List[Int] = {
    @tailrec
    def extendFibonacci(l: List[Int], n: Int): List[Int] =
      if (n == 0) l
      else extendFibonacci(l :+ (l.last + l.init.last), n - 1)

    extendFibonacci(List(1, 1), 98)
  }

  def P13OtherFibonacci01(): Unit = {
    print(f"1 1")
    var x_1 = 1
    var x_2 = 1
    for (_ <- 3 to 101) {
      print(f" ${x_2 + x_1}")
      x_1 += x_2
      x_2 = x_1 - x_2
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"P13 L 01: ${P13ListFibonacci01()}")
    println(s"P13 O 01: ")
    P13OtherFibonacci01() // Int overflow
  }
}
