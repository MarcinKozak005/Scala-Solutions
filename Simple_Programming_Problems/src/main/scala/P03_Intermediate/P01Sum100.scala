package P03_Intermediate

object P01Sum100 {

  def P01Sum100(numbers: List[Int], operations: List[String]): Unit = numbers match {
    case result :: Nil if result == 100 =>
      (1 to 9).toList zip (operations :+ " = 100\n") foreach (t => print(f"${t._1}${t._2}"))
    case _ :: Nil => // result != 100
    case x :: y :: tail =>
      P01Sum100((x + y) +: tail, operations :+ "+")
      P01Sum100((x - y) +: tail, operations :+ "-")
      P01Sum100((10 * x + y) +: tail, operations :+ "")
  }

  def main(args: Array[String]): Unit = {
    P01Sum100((1 to 9).toList, List())
  }

}
