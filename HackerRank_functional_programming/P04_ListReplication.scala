object P04_ListReplication {
  def f(n:Int, arr:List[Int]): List[Int] = {
    for (e <- arr; _ <- 1 to n)  yield e
  }
}
