package Y2015

import java.security.MessageDigest
import scala.annotation.tailrec

object D04 {

  private val secretKey = "bgvyzdsv"
  private val md = MessageDigest.getInstance("MD5")

  def P01(): Int = calculateMD5BeginningWith(0, "0" * 5)

  def P02(): Int = calculateMD5BeginningWith(0, "0" * 6)

  @tailrec
  def calculateMD5BeginningWith(number: Int, prefix: String): Int = {
    if (md.digest(f"$secretKey$number".getBytes).map("%02x".format(_)).mkString.startsWith(prefix))
      number
    else calculateMD5BeginningWith(number + 1, prefix)
  }

  def main(args: Array[String]): Unit = {
        println(P01())
    println(P02())
  }

}
