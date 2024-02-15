class P11_SecretHandshake {}

object SecretHandshake {

  private val actionsBitOrder = List("jump", "close your eyes", "double blink", "wink")

  private def stripLast4Bits(number: Int): String = (number + 16).toBinaryString.takeRight(4)

  def commands(number: Int): List[String] = {
    val result = stripLast4Bits(number).zip(actionsBitOrder).filter(_._1 == '1').map(_._2)
    (if (number >= 16) result else result.reverse).toList
  }
}