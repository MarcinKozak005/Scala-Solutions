import scala.io.StdIn.readLine

object Solution {
  def main(args: Array[String]): Unit = {
    val p = readLine()
    val q = readLine()
    println(p.zip(q).flatten(p => s"${p._1}${p._2}").mkString(""))
  }
}
