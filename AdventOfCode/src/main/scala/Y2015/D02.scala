package Y2015

import scala.io.Source

object D02 {

  def P01(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D02.txt")
    val result = for {
      line <- data.getLines()
      Array(x, y, z) = line.split("x").map(_.toInt)
    } yield 2 * x * y + 2 * x * z + 2 * y * z + math.min(x * y, math.min(x * z, y * z))
    result.sum
  }

  def P02(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D02.txt")
    val result = for {
      line <- data.getLines()
      Array(x, y, z) = line.split("x").map(_.toInt)
    } yield 2 * ((x + y + z) - math.max(x, math.max(y, z))) + (x * y * z)
    result.sum
  }

  def main(args: Array[String]): Unit = {
    println(P01())
    println(P02())
  }

}
