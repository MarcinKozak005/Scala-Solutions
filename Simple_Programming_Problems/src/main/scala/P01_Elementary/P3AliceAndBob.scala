package P01_Elementary

import scala.io.StdIn.readLine

object P3AliceAndBob {
  def P3AliceAndBob(): Unit = {
    println("What is Your name?")
    val name = readLine()
    if (name.equals("Alice") || name.equals("Bob"))
      println("Hello " + name + "!")
    else
      println("Hello!")
  }
}
