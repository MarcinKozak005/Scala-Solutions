class P34_PhoneNumber {

}


object PhoneNumber {
  private def checkSpecialPositions(numberNoCountryCode: String): Boolean =
    numberNoCountryCode(0).asDigit >= 2 && numberNoCountryCode(3).asDigit >= 2

  def clean(inputNumber: String): Option[String] = {
    val cleaned = inputNumber.replaceAll("[^0-9]", "")
    if (cleaned.length == 10 && checkSpecialPositions(cleaned)) Some(cleaned)
    else if (cleaned.length == 11 && cleaned(0).asDigit == 1 && checkSpecialPositions(cleaned.tail)) Some(cleaned.tail)
    else None
  }
}