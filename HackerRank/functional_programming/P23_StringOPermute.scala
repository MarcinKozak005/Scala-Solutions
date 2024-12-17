import scala.io.StdIn._

object Solution {

  def oPermute(str: String): String = {
    val original = str.trim()
    val res = for {
      i <- 0 until original.length()
    } yield {
      if (i % 2 == 0) original.charAt(i + 1)
      else original.charAt(i - 1)
    }
    res.mkString("")
  }

  def main(args: Array[String]): Unit = {
    (1 to readInt()).foreach { _ =>
      println(oPermute(readLine()))
    }
  }
}
