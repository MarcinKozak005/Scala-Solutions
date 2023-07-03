import scala.io.StdIn._
import scala.math.abs
import scala.annotation.tailrec

object Solution {

  def calculateArea(pairs: List[(Int, Int)]): Double = {
    // scholace method
    def calculatePart(a: (Int, Int), b: (Int, Int)): Double =
      (a._1 * b._2 - a._2 * b._1).toDouble

    @tailrec
    def scholaceMethod(points: List[(Int, Int)], area: Double): Double =
      points match {
        case _ :: Nil => area
        case a :: b :: tail =>
          scholaceMethod(b :: tail, area + calculatePart(a, b))
      }
    abs(scholaceMethod(pairs :+ pairs.head, 0.0)) / 2
  }

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val pairs = for {
      i <- 1 to n
    } yield {
      val input = readLine().split(" ").map(_.toInt)
      (input(0), input(1))
    }
    println(calculateArea(pairs.toList))

  }
}
