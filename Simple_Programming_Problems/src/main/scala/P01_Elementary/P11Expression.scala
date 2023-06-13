package P01_Elementary

object P11Expression extends App {
  def solution1(): Unit = {
    var result = 0.0
    for (k <- 1 to 1_000_000) {
      result += (math.pow(-1, k + 1) / (2 * k - 1))
    }
    println(s"Result of computation is equal: ${4 * result}")
  }

  def solution2(): Unit =
    println(s"Result of computation is equal: ${
      4 * (1 to 1_000_000).foldLeft(0.0) {
        (sum, k) => sum + (math.pow(-1, k + 1) / (2 * k - 1))
      }
    }")
}
