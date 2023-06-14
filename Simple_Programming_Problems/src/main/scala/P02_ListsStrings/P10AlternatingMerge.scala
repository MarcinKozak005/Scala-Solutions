package P02_ListsStrings

object P10AlternatingMerge {
  def P10ListAlternatingMerge01(list1: List[Any], list2: List[Any]): List[Any] = {
    def takeFromList(list1: List[Any], list2: List[Any]): List[Any] = list1 match {
      case Nil => list2
      case x :: tail => x :: takeFromList(list2, tail)
    }

    takeFromList(list1, list2)
  }

  def P10ListAlternatingMerge02(list1: List[Any], list2: List[Any]): List[Any] =
    list1 zip list2 flatten { case (a, b) => List(a, b) }

  def P10ArrayAlternatingMerge01(array1: Array[Any], array2: Array[Any]): Array[Any] = {
    val result = new Array[Any](array1.length + array2.length)
    for (i <- 0 until array1.length + array2.length) {
      if (i % 2 == 0) {
        result(i) = array1(i / 2)
      }
      else {
        result(i) = array2((i - 1) / 2)
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(s"P10 L 01: ${P10ListAlternatingMerge01(List('a', 'b', 'c'), List(1, 2, 3))}")
    println(s"P10 L 02: ${P10ListAlternatingMerge02(List('a', 'b', 'c'), List(1, 2, 3))}")
    println(s"P10 A 01: ${P10ArrayAlternatingMerge01(Array('a', 'b', 'c'), Array(1, 2, 3)).mkString(" ")}")

  }
}
