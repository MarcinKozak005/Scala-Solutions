package P02_ListsStrings

object P07Sum {
  def P07Sum01(list: List[Int]): Int = {
    var result = 0
    for (i <- list.indices) result += list(i)
    result
  }

  def P07Sum02(list: List[Int]): Int = {
    var result = 0
    for (e <- list) result += e
    result
  }

  def P07Sum03(list: List[Int]): Int = {
    var i = 0
    var result = 0
    while (i < list.length) {
      result += list(i)
      i += 1
    }
    result
  }

  def P07Sum04(list: List[Int]): Int = {
    def sumElem(list: List[Int]): Int = {
      list match {
        case Nil => 0
        case x :: tail => x + sumElem(tail)
      }
    }
    sumElem(list)
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P07 L 01: ${P07Sum01(seq.toList)}")
    println(f"P07 L 02: ${P07Sum02(seq.toList)}")
    println(f"P07 L 03: ${P07Sum03(seq.toList)}")
    println(f"P07 L 04: ${P07Sum04(seq.toList)}")
  }
}
