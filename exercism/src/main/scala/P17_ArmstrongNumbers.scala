import scala.math.pow

class P17_ArmstrongNumbers {

}


object ArmstrongNumbers {
  def isArmstrongNumber(number: Int): Boolean = {
    val numberString = number.toString
    numberString.split("").map(n => pow(n.toInt, numberString.length).toInt).sum == number
  }
}