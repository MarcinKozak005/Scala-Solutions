import scala.math.abs
def f(arr:List[Int]):List[Int] = for (e <- arr) yield abs(e)