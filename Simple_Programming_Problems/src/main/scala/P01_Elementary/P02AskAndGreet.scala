package P01_Elementary

import scala.io.StdIn.readLine

object P02AskAndGreet {
  def P2AskAndGreet(): Unit = {
    println("What is Your name?")
    println("Hello " + readLine() + "!")
  }
}