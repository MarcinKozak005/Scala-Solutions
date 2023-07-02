import scala.io.StdIn.readInt

object Solution {

  def pascalTriangleRow(n: Int): List[Int] = n match {
    case 1 => List(1)
    case _ => {
      val prev = pascalTriangleRow(n-1)
      (0 +: prev).zip(prev :+ 0).map(pair => pair._1 + pair._2)
    }
  }

  def pascalTriangle(n: Int): Unit = (1 to n).foreach { i =>
    println(pascalTriangleRow(i).mkString(" "))
  }

  def main(args: Array[String]): Unit = {
    pascalTriangle(readInt())
  }
}
