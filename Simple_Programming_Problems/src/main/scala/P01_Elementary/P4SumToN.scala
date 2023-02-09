package P01_Elementary

import scala.io.StdIn.readLine

object P4SumToN {
  def P4SumToN(): Unit = {
    print("Enter a number: ")
    val n = readLine()
    println("Sum of numbers 1 to " + n + " is: " + List.range(1, n.toInt).sum)
  }
}
