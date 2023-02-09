package P02_ListsStrings

object P03Contains {
  def P03ListContains01(list: List[Int], elem: Int): Boolean = list.contains(elem)

  def P03ListContains02(list: List[Int], elem: Int): Boolean =
    list match {
      case Nil => false
      case x :: _ => x == elem
    }

  def P03ArrayContains01(array: Array[Int], elem: Int): Boolean = array.contains(elem)

  def P03ArrayContains02(array: Array[Int], elem: Int): Boolean = {
    var result = false
    for (e <- array) result = result || (e == elem)
    result
  }

  def P03ArrayContains03(array: Array[Int], elem: Int): Boolean = {
    array.contains(elem)
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P03 L 01: ${P03ListContains01(seq.toList, 2)}")
    println(f"P03 L 02: ${P03ListContains02(seq.toList, 2)}")
    println(f"P03 A 01: ${P03ArrayContains01(seq.toArray, 2)}")
    println(f"P03 A 02: ${P03ArrayContains02(seq.toArray, 2)}")
    println(f"P03 A 03: ${P03ArrayContains03(seq.toArray, 2)}")
  }
}
