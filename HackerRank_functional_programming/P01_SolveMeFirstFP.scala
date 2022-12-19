import scala.io.StdIn

object P01_SolveMeFirstFP {
  def main(args: Array[String]) {
    // println(io.Source.stdin.getLines().take(2).map(_.toInt).sum)
    println(StdIn.readLine.toInt + StdIn.readLine.toInt)
  }
}
