package P02_ListsStrings

object P01Largest {
  def P01ListLargest01(list: List[Int]): Int = list.max

  def P01ListLargest02(list: List[Int]): Int = {
    var maxElem = list.head
    for (e <- list) {
      maxElem = if (e > maxElem) e else maxElem
    }
    maxElem
  }

  def P01ArrayLargest01(array: Array[Int]): Int = array.max

  def P01ArrayLargest02(array: Array[Int]): Int = {
    var maxElem = array(0)
    for (e <- array) {
      maxElem = if (e > maxElem) e else maxElem
    }
    maxElem
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P01 L 01: ${P01ListLargest01(seq.toList)}")
    println(f"P01 L 02: ${P01ListLargest02(seq.toList)}")
    println(f"P01 A 01: ${P01ArrayLargest01(seq.toArray)}")
    println(f"P01 A 02: ${P01ArrayLargest02(seq.toArray)}")
  }
}
