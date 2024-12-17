import scala.io.StdIn
import scala.math._

object Solution {

  def distance(p1: (Int, Int), p2: (Int, Int)): Double = sqrt(
    pow(abs(p1._1 - p2._1), 2) + pow(abs(p1._2 - p2._2), 2)
  )

  def computeDistance(list: List[(Int, Int)]): Double =
    computeDistanceHelper(list.head, list.tail, distance(list.head, list.last))

  def computeDistanceHelper(
      point: (Int, Int),
      list: List[(Int, Int)],
      result: Double
  ): Double = list match {
    case head :: tail =>
      computeDistanceHelper(
        head,
        tail,
        result + distance(point, head)
      )
    case Nil => result
  }

  def main(args: Array[String]): Unit = {
    val numberOfPoints = StdIn.readLine().toInt
    val points = for {
      _ <- 1 to numberOfPoints
    } yield {
      val point = StdIn.readLine().split(" ")
      (point(0).toInt, point(1).toInt)
    }
    println(computeDistance(points.toList))

  }
}
