import scala.io.StdIn

object Solution {

  def processTestCases(num: Int): Unit = num match {
    case 0 => {}
    case _ => {
      val n = StdIn.readLine().toInt
      val values = readNPoints(n).toArray
      println((if (isFunction(values)) "YES" else "NO"))
      processTestCases(num - 1)
    }
  }

  def readNPoints(n: Int): List[(Int, Int)] = n match {
    case 0 => List()
    case _ => {
      val pair = StdIn.readLine().split(" ")
      (pair(0).toInt, pair(1).toInt) :: readNPoints(n - 1)
    }
  }

  def isFunction(tab: Array[(Int, Int)]): Boolean = {
    val domain = tab.map(_._1).toSet
    domain.size == tab.size
  }

  def main(args: Array[String]): Unit = processTestCases(StdIn.readLine().toInt)

}
