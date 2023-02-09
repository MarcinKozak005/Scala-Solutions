package P02_ListsStrings

object P17SelectionSort {
  def P17ListSelectionSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case _ =>
      val minimal = list.min
      val indexOfMinimal = list.indexOf(minimal)
      minimal +: P17ListSelectionSort(list.slice(0, indexOfMinimal) ::: list.slice(indexOfMinimal + 1, list.length))
  }

  def P17ListInsertionSort(list: List[Int]): List[Int] = {
    def insertionSortHelper(list: List[Int]): List[Int] = {
      val pos = list.length - 1
      val i = list.indexWhere(elem => elem > list(pos))
      if (i >= 0) {
        (list.slice(0, i) :+ list(pos)) ::: list.slice(i, pos) ::: list.slice(pos + 1, list.length)
      } else list
    }

    var result = list
    for (i <- 0 to list.length) {
      result = insertionSortHelper(result.slice(0, i + 1)) ::: result.slice(i + 1, result.length)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(f"P17 L SelectionSort: ${P17ListSelectionSort(List(5, 3, 2, 4, 1))}")
    println(f"P17 L InsertionSort: ${P17ListInsertionSort(List(3, 2, 1, 32, 11, 23))}")

  }
}
