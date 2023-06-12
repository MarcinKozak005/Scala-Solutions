package P01_Elementary

import scala.io.StdIn.readLine

object P03AliceAndBob extends App {
  println("What is Your name?")
  val name = readLine()

  def solution1(): Unit = if (name.equals("Alice") || name.equals("Bob"))
    println(s"Hello $name !")
  else
    println("Hello!")

  def solution2(): Unit = name match {
    case n if List("Alice", "Bob") contains n => println(s"Hello $name !")
    case _ => print("Hello!")
  }
  solution1()
  solution2()
}
