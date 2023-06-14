package P02_ListsStrings

object P11MergeSorted {
  def P11ListMergeSorted01(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1 == Nil) {
      list2
    } else if (list2 == Nil) {
      list1
    } else {
      if (list1.head <= list2.head) {
        list1.head +: P11ListMergeSorted01(list1.tail, list2)
      } else {
        list2.head +: P11ListMergeSorted01(list1, list2.tail)
      }
    }
  }

  def P11ArrayMergeSorted01(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    val result = new Array[Int](array1.length + array2.length)
    var i = 0
    var j = 0
    while (i < array1.length && j < array2.length) {
      if (array1(i) <= array2(j)) {
        result(i + j) = array1(i)
        i += 1
      } else {
        result(i + j) = array2(j)
        j += 1
      }
    }
    while (i < array1.length) {
      result(i + j) = array1(i)
      i += 1
    }
    while (j < array2.length) {
      result(i + j) = array2(j)
      j += 1
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(s"P11 L 01: ${P11ListMergeSorted01(List(1, 4, 6), List(2, 3, 5))}")
    println(s"P11 A 01: ${P11ArrayMergeSorted01(Array(1, 4, 6), Array(2, 3, 5)).mkString(" ")}")

  }
}
