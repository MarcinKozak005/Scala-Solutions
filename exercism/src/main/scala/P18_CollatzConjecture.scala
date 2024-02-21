import scala.annotation.tailrec

class P18_CollatzConjecture {

}

object CollatzConjecture {
  def steps(number: Int): Option[Int] = computeCollatz(number, 0)

  @tailrec
  private def computeCollatz(number: Int, steps: Int): Option[Int] = number match {
    case nonPositive if nonPositive <= 0 => None
    case 1 => Some(steps)
    case even if even % 2 == 0 => computeCollatz(even / 2, steps + 1)
    case odd => computeCollatz(3 * odd + 1, steps + 1)
  }

}