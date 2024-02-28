class P33_Grains {

}


object Grains {

  def square(n: Int): Option[BigInt] = {
    if (n <= 0 || n > 64) None
    else Option(BigInt(2).pow(n - 1))

  }

  def total: BigInt = (1 to 64).map(square).collect { case Some(bigInt) => bigInt }.sum

}