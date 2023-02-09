package P02_ListsStrings

object P04OddPositions {
  def P04ListOddPositions01(list: List[Int]): List[Int] = {
    var result = List[Int]()
    for (i <- list.indices) {
      if (i % 2 == 1) {
        result = result :+ list(i)
      }
    }
    result
  }

  def P04ListOddPositions02(list: List[Int]): List[Int] = {
    def takeElem(list: List[Int]): List[Int] = list match {
      case Nil => Nil
      case e :: tail => e +: dontTakeElem(tail)
    }

    def dontTakeElem(list: List[Int]): List[Int] = list match {
      case Nil => Nil
      case e :: tail => takeElem(tail)
    }

    dontTakeElem(list)
  }

  def P04ArrayOddPositions01(array: Array[Int]): Array[Int] = {
    var result = Array[Int]()
    for (i <- array.indices) {
      if (i % 2 == 1) {
        result = result :+ array(i)
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(f"P04 L 01: ${P04ListOddPositions01(seq.toList)}")
    println(f"P04 L 02: ${P04ListOddPositions02(seq.toList)}")
    println(f"P04 A 01: ${P04ArrayOddPositions01(seq.toArray).mkString(" ")}")
  }
}
