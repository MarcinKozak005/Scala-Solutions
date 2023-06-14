package P02_ListsStrings

object P08OnAll {
  // on the website it's called: on_all
  def P08ListOnAll01(list: List[Int], fun: Int => Int): List[Int] = list.map(fun)

  // on the website it's called: on_all
  def P08ArrayOnAll01(array: Array[Int], fun: Int => Int): Array[Int] = {
    val result = new Array[Int](array.length)
    for (i <- array.indices) {
      result(i) = fun(array(i))
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(s"P08 L 01: ${P08ListOnAll01(List.range(1, 21), x => x * x)}")
    println(s"P08 A 01: ${P08ArrayOnAll01(List.range(1, 21).toArray, x => x * x).mkString(" ")}")
  }
}
