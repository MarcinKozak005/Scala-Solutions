package P02_ListsStrings

import scala.annotation.tailrec

object P18BinarySearch {
  def P18ListBinarySearch(list: List[Int], elem: Int): Boolean = {
    @tailrec
    def P18ListBinarySearchHelper(list: List[Int], elem: Int, start: Int, end: Int): Boolean = start + (end - start) / 2 match {
      case middle if list(middle) > elem && end - start != 0 => P18ListBinarySearchHelper(list, elem, start, middle - 1)
      case middle if list(middle) < elem && end - start != 0 => P18ListBinarySearchHelper(list, elem, middle + 1, end)
      case middle if list(middle) == elem => true
      case _ => false
    }

    P18ListBinarySearchHelper(list, elem, 0, list.length - 1)
  }

  def main(args: Array[String]): Unit = {
    println(f"P18 L 01: ${P18ListBinarySearch(List(3, 2, 1, 32, 11, 23).sorted, 100)}")
  }
}
