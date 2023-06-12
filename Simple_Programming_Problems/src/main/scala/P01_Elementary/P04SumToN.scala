package P01_Elementary

import scala.io.StdIn.readLine

object P04SumToN extends App {
  print("Enter a number: ")
  val n = readLine()
  println(s"Sum of numbers 1 to $n is: ${(1 to n.toInt).sum}")
}
