package P02_ListsStrings

object P17Sortings {
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

  def P17ListMergeSort(list: List[Int]): List[Int] = {
    def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (_, Nil) => left
      case (Nil, _) => right
      case (l :: lTail, r :: rTail) => if (l < r) l :: merge(lTail, right) else r :: merge(left, rTail)
    }

    def divide(list: List[Int]): List[Int] = list match {
      case Nil => Nil
      case _ :: Nil => list
      case _ =>
        val left = list.slice(0, list.length / 2)
        val right = list.slice(list.length / 2, list.length)
        merge(divide(left), divide(right))
    }

    divide(list)
  }

  def P17ListQuickSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case h :: tail => P17ListQuickSort(tail.filter(_ < h)) ::: List(h) ::: P17ListQuickSort(tail.filter(_ >= h))
  }

  def P17StoogeSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case _ :: Nil => list
    case List(fst, snd) => if (fst > snd) List(snd, fst) else List(fst, snd)
    case _ =>
      val divider = math.ceil(2 * list.length / 3.0).toInt
      val l1 = P17StoogeSort(list.slice(0, divider)) ::: list.slice(divider, list.length)
      val l2 = l1.slice(0, l1.length - divider) ::: P17StoogeSort(l1.slice(l1.length - divider, l1.length))
      P17StoogeSort(l2.slice(0, divider)) ::: l2.slice(divider, l2.length)
  }

  def main(args: Array[String]): Unit = {
    println(s"P17 L SelectionSort: ${P17ListSelectionSort(List(5, 3, 2, 4, 1))}")
    println(s"P17 L InsertionSort: ${P17ListInsertionSort(List(3, 2, 1, 32, 11, 23))}")
    println(s"P17 L MergeSort: ${P17ListMergeSort(List(3, 2, 1, 32, 11, 23))}")
    println(s"P17 L QuickSort: ${P17ListQuickSort(List(3, 2, 1, 32, 11, 23))}")
    println(s"P17 L StoogeSort: ${P17StoogeSort(List(3, 2, 1, 32, 11, 23))}")
  }
}
