package P01_Elementary

object P11Expression {
  def P11Expression(): Unit = {
    var result = 0.0
    for (k <- 1 to 1_000_000) {
      result += (math.pow(-1, k + 1) / (2 * k - 1))
    }
    println(f"Result of computation is equal: ${4 * result}")
  }
}
