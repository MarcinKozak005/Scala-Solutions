package P02_ListsStrings

import P02_ListsStrings.P14SplitNumber.P14ListSplitNumber

object P16ChangeBase {
  def P16ListChangeBase(list: List[Int], base1: Int, base2: Int): List[Int] = {
    P14ListSplitNumber(
      java.lang.Long.toString(
        java.lang.Long.parseLong(list.map(_.toString).mkString, base1),
        base2
      ).mkString.toInt
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"P16 L 01: ${P16ListChangeBase(List(1, 2, 3), 10, 5)}")
  }
}
