package P02_ListsStrings

object P02Reverse {
  def P02ListReverse01(list: List[Int]): List[Int] = list.reverse

  def P02ListReverse02(list: List[Int]): List[Int] =
    list match {
      case Nil => Nil
      case x :: tail => P02ListReverse02(tail) :+ x
    }

  def P02ArrayReverse01(array: Array[Int]): Array[Int] = array.reverse

  def P02ArrayReverse02(array: Array[Int]): Array[Int] = {
    var tmp: Int = 0
    for (i <- 0 until (array.length + 1) / 2) {
      tmp = array(i)
      array(i) = array((array.length - 1) - i)
      array((array.length - 1) - i) = tmp
    }
    array
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P02 L 01: ${P02ListReverse01(seq.toList)}")
    println(f"P02 L 02: ${P02ListReverse02(seq.toList)}")
    println(f"P02 A 01: ${P02ArrayReverse01(seq.toArray).mkString(" ")}")
    println(f"P02 A 02: ${P02ArrayReverse02(seq.toArray).mkString(" ")}")
  }
}
