package P02_ListsStrings

object P14SplitNumber {
  def P14ListSplitNumber(number: Int): List[Int] = number.toString.map(x => x.toInt - 48).toList

  def P14ArraySplitNumber(number: Int): Array[Int] = number.toString.map(x => x.toString.toInt).toArray

  def main(args: Array[String]): Unit = {
    println(f"P14 L 01: ${P14ListSplitNumber(2342)}")
    println(f"P14 A 01: ${P14ArraySplitNumber(2342).mkString(" ")}")
  }
}