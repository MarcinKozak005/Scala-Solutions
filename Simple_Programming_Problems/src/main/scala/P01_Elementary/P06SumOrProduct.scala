package P01_Elementary

import scala.io.StdIn.readLine

object P06SumOrProduct {
  def P6SumOrProduct(): Unit = {
    print("Enter a number: ")
    val n = readLine()
    println("What do You want to compute?\n" +
      "Enter S for sum of numbers 1-" + n + "\n" +
      "Enter P for product of numbers 1-" + n)
    val input = readLine()
    if (input.equals("S")) println("Sum of 1-" + n + " is: " + List.range(1, n.toInt).sum)
    else if (input.equals("P")) println("Product of 1-" + n + " is: " + List.range(1, n.toInt).product)
  }
}
