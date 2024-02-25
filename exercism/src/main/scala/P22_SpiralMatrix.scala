class P22_SpiralMatrix {

}

object SpiralMatrix {
  private val Matrix0: List[List[Int]] = List()
  private val Matrix1 = List(List(1))
  private val Matrix2 = List(
    List(1, 2),
    List(4, 3)
  )
  private val Matrix3 = List(
    List(1, 2, 3),
    List(8, 9, 4),
    List(7, 6, 5)
  )

  private val BaseCases = List(Matrix0, Matrix1, Matrix2, Matrix3)

  def spiralMatrix(size: Int): List[List[Int]] = size match {
    case n@(0 | 1 | 2 | 3) => BaseCases(n)
    case n =>
      val innerMatrix = spiralMatrix(n - 2)
      val firstRow = List.from(1 to n)
      val lastRow = List.from((3 * n - 2) to (2 * n - 1) by -1)
      val innerRowsFirstNumberBase = 4 * n - 3
      val intermediateRows: List[List[Int]] = (1 to n - 2).map { row =>
        val firstElem = innerRowsFirstNumberBase - row
        val lastElem = n + row
        val intermediateElements = innerMatrix(row - 1).map(_ + innerRowsFirstNumberBase - 1)
        firstElem +: intermediateElements :+ lastElem
      }.toList
      firstRow +: intermediateRows :+ lastRow

  }
}
