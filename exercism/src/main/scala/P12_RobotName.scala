import RobotP12.{Cache, getRandomName}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{max, pow}
import scala.util.Random

class P12_RobotName {}

/*
General approach:
Assume: L-letter and D-digit random string (L=2, D=3, ex: "CD123")
There are 26 letters in English alphabet
Lets assign each possible string a number from range 0 to 26^L * 10^D (exclusive)
---
AA000 -> 0
AA001 -> 1
...
ZZ999 -> 675 999
---
We can represent each such string as a hybrid (concatenation) of:
1) L-"digit" base 26 number, where each "digit" is a letter A-Z:
---
A -> 0 (AA -> 0 [with padding])
B -> 1 (AB -> 1 [with padding])
...
Z -> 25 (AZ -> 25 [with padding])
---
2) and D-digit base 10 number

Some examples which might help:
---
AA999 -> 999
AB000 -> 1000
AZ999 -> 25999
BA000 -> 26000
---
*/

class RobotP12 {

  var (name, nameIntRepresentation) = getRandomName

  def reset(): Unit = {
    Cache.addOne(nameIntRepresentation)
    val newValue = getRandomName
    name = newValue._1
    nameIntRepresentation = newValue._2
  }
}


object RobotP12 {
  private val NumberOfLetters = 2
  private val NumberOfDigits = 3
  private val LettersBase = 'Z'.toInt - 'A'.toInt + 1
  private val NumericRange = (pow(LettersBase, NumberOfLetters) * pow(10, NumberOfDigits)).toInt
  private val RandomGen = Random
  private val Cache = mutable.TreeSet.from(0 to NumericRange)
  private val NumOfTries = 3

  private def getRandomInt: Int = {
    @tailrec
    def drawRandomOrFirstUnassigned(triesLeft: Int): Int = triesLeft match {
      case 0 => Cache.head
      case _ =>
        val randomInt = RandomGen.between(0, NumericRange)
        if (Cache.contains(randomInt)) randomInt else drawRandomOrFirstUnassigned(triesLeft - 1)
    }

    val result = drawRandomOrFirstUnassigned(NumOfTries)
    Cache.remove(result)
    result
  }

  def getRandomName: (String, Int) = {
    val randomInt = getRandomInt
    val randomStr = ("0" * (NumberOfLetters + NumberOfDigits - randomInt.toString.length)) + randomInt.toString
    val (prefixNumericString, suffixNumericString) = randomStr.splitAt(randomStr.length - NumberOfDigits)
    (paddedBase26Letter(prefixNumericString.toInt, NumberOfLetters) + suffixNumericString, randomInt)
  }

  def paddedBase26Letter(number: Int, numberWidth: Int = 0): String = {
    def toLetterBased26(n: Int): String = (n + 'A'.toInt).toChar.toString

    @tailrec
    def convertToBase26LetterTail(number: Int, result: String): String = {
      val (quotient, remainder) = (number / LettersBase, number % LettersBase)
      if (quotient == 0) toLetterBased26(remainder) + result
      else convertToBase26LetterTail(quotient, toLetterBased26(remainder) + result)
    }

    val base26LetterNumber = convertToBase26LetterTail(number, "")
    "A" * max(0, numberWidth - base26LetterNumber.length) + base26LetterNumber
  }

}
