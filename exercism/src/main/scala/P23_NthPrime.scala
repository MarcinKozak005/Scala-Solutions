import scala.annotation.tailrec
import scala.math.{ceil, sqrt}

class P23_NthPrime {

}

object NthPrime {

  def prime(n: Int): Option[Int] = if (n == 0) None else Some(primeHelper(n, 2, 0))

  @tailrec
  private def primeHelper(n: Int, currentNumber: Int, primeCounter: Int): Int =
    if (isPrime(currentNumber)) {
      primeCounter + 1 match {
        case newCounterValue if newCounterValue == n => currentNumber
        case _ => primeHelper(n, currentNumber + 1, primeCounter + 1)
      }
    } else primeHelper(n, currentNumber + 1, primeCounter)

  private def isPrime(number: Int): Boolean = if (number == 2) true else isPrimeHelper(number, 2)

  @tailrec
  private def isPrimeHelper(number: Int, currentDivisor: Int): Boolean = number % currentDivisor match {
    case 0 => false
    case _ if currentDivisor == ceil(sqrt(number)) => true
    case _ => isPrimeHelper(number, currentDivisor + 1)
  }
}
