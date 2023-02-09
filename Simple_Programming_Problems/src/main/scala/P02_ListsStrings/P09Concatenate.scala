package P02_ListsStrings

object P09Concatenate {
  def P09ListConcatenate01(list1: List[Any], list2: List[Any]): List[Any] = list1 ::: list2

  def P09ArrayConcatenate01(array1: Array[Any], array2: Array[Any]): Array[Any] = array1 ++ array2

  def main(args: Array[String]): Unit = {
    println(f"P09 L 01: ${P09ListConcatenate01(List('a', 'b', 'c'), List(1, 2, 3))}")
    println(f"P09 A 01: ${P09ArrayConcatenate01(Array('a', 'b', 'c'), Array(1, 2, 3)).mkString(" ")}")
  }
}
