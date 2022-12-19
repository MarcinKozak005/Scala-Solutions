object P03_HelloWorldNTimes {
  // My code
  def f(n: Int) = for (i <- 1 to n) {
    println("Hello World")
  }
  // End of my code

  var n = scala.io.StdIn.readInt
  f(n)
}
