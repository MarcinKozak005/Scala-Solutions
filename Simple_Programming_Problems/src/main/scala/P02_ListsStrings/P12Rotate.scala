package P02_ListsStrings

import scala.annotation.tailrec

object P12Rotate {
  @tailrec
  def P12ListRotate01(list: List[Any], k: Int): List[Any] = {
    val shift = k % list.length
    shift match {
      case 0 => list
      case x if x > 0 => P12ListRotate01(list.tail :+ list.head, x - 1)
    }
  }

  def P12ListRotate02(list: List[Any], k: Int): List[Any] = list.drop(k) ::: list.take(k)

  def P12ArrayRotate01(array: Array[Int], k: Int): Array[Int] = {
    val shift = k % array.length
    Array.concat(array.slice(shift, array.length), array.slice(0, shift))
  }

  def main(args: Array[String]): Unit = {
    val seq = Seq(3, 2, 1, 32, 11, 23)
    println(s"P12 L 01: ${P12ListRotate01(seq.toList, 2)}")
    println(s"P12 L 02: ${P12ListRotate02(seq.toList, 2)}")
    println(s"P12 A 01: ${P12ArrayRotate01(seq.toArray, 2).mkString(" ")}")
  }
}
