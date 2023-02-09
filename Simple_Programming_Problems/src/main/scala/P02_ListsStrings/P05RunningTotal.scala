package P02_ListsStrings

object P05RunningTotal {
  def P05ListRunningTotal01(list: List[Int]): Int = list.sum

  def P05ListRunningTotal02(list: List[Int]): Int = list.foldLeft(0)(_ + _)

  def P05ListRunningTotal03(list: List[Int]): Int = list.reduce(_ + _)

  def P05ArrayRunningTotal01(array: Array[Int]): Int = {
    var result = 0
    for (e <- array) result += e
    result
  }

  def P05ArrayRunningTotal02(array: Array[Int]): Int = {
    // In Java such construct does not work
    var result = 0
    array.foreach(result += _)
    result
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P05 L 01: ${P05ListRunningTotal01(seq.toList)}")
    println(f"P05 L 02: ${P05ListRunningTotal02(seq.toList)}")
    println(f"P05 L 03: ${P05ListRunningTotal03(seq.toList)}")
    println(f"P05 A 01: ${P05ArrayRunningTotal01(seq.toArray)}")
    println(f"P05 A 02: ${P05ArrayRunningTotal02(seq.toArray)}")

  }
}
