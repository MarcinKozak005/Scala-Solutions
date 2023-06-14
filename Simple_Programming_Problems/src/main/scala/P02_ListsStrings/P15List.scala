package P02_ListsStrings

import P02_ListsStrings.P14SplitNumber.{P14ArraySplitNumber, P14ListSplitNumber}

object P15List {
  def P15List(list1: List[Int], list2: List[Int], f: (Int, Int) => Int): List[Int] =
    P14ListSplitNumber(
      f(
        list1.map(_.toString).mkString.toInt,
        list2.map(_.toString).mkString.toInt
      )
    )

  def P15Array(array1: Array[Int], array2: Array[Int], f: (Int, Int) => Int): Array[Int] =
    P14ArraySplitNumber(f(array1.map(_.toString).mkString.toInt, array2.map(_.toString).mkString.toInt))

  def main(args: Array[String]): Unit = {
    val seq123 = Seq(1, 2, 3)
    val seq321 = Seq(3, 2, 1)
    println(s"P15 L Part01: ${P15List(seq321.toList, seq123.toList, _ + _)}")
    println(s"P15 L Part02: ${P15List(seq321.toList, seq123.toList, _ - _)}")
    println(s"P15 L Part03: ${P15List(seq321.toList, seq123.toList, _ * _)}")
    println(s"P15 A Part01: ${P15Array(seq321.toArray, seq123.toArray, _ + _).mkString}")
    println(s"P15 A Part02: ${P15Array(seq321.toArray, seq123.toArray, _ - _).mkString}")
    println(s"P15 A Part03: ${P15Array(seq321.toArray, seq123.toArray, _ * _).mkString}")
  }

}
