import scala.annotation.tailrec
import scala.collection.immutable.Queue

class P21_PrimeFactors {

}


object PrimeFactors {
  def factors(number: Long): List[Int] = factorStep(number, 2, Queue())

  @tailrec
  def factorStep(number: Long, divisor: Int, result: Queue[Int]): List[Int] =
    if (number <= 1) result.toList
    else number % divisor match {
      case 0 => factorStep(number / divisor, divisor, result.appended(divisor))
      case _ => factorStep(number, divisor + 1, result)
    }
}