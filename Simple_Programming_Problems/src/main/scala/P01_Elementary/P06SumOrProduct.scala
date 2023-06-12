package P01_Elementary

import scala.io.StdIn.readLine

object P06SumOrProduct extends App {
  print("Enter a number: ")
  val n = readLine()
  val list = 1 to n.toInt
  println(
    s"""
      |What do You want to compute?
      |Enter S for sum of numbers 1 to $n
      |Enter P for product of numbers 1 to $n
      |""".stripMargin)
  readLine() match {
    case "S" => println(s"Sum of 1 to $n is: ${list.sum}")
    case "P" => println(s"Product of 1 to $n is: ${list.product}")
  }
}
