package P01_Elementary

object P7MultiplicationTable {
  def P7MultiplicationTable(): Unit = {
    for (i <- 1 to 12) {
      for (j <- 1 to 12) {
        printf("%3d ", i * j)
      }
      println()
    }
  }
}
