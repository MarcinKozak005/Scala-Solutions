package P02_ListsStrings

object P06IsPalindrome {
  def P06IsPalindrome01(str: String): Boolean = str.equals(str.reverse)

  def P06IsPalindrome02(str: String): Boolean = {
    var result = true
    for (i <- 0 until str.length) {
      result = result && str(i) == str(str.length - 1 - i)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(f"P06 S 01: ${P06IsPalindrome01("eye")}")
    println(f"P06 S 02: ${P06IsPalindrome02("eye")}")
  }
}
