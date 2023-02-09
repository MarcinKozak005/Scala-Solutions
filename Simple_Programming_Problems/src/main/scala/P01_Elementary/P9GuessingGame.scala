package P01_Elementary

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import scala.util.Random

object P9GuessingGame {
  def P9GuessingGame(): Unit = {
    val numberToGuess = Random.nextInt()
    var numberOfTries = 0
    val triedNumbers = ArrayBuffer[Int]()
    var guessedNumber = 0
    var game = true
    while (game) {
      print("Guess the secret number:")
      guessedNumber = readLine().toInt
      numberOfTries += 1
      if (guessedNumber == numberToGuess) {
        println(f"The number was: $numberToGuess and You guessed it in $numberOfTries tries!")
        game = false
      }
      else {
        if (triedNumbers.contains(guessedNumber)) {
          print("You already guessed this number. ")
        }
        if (guessedNumber < numberToGuess) println("Enter a greater number")
        else println("Enter a lesser number")
        triedNumbers += guessedNumber
      }
    }
  }
}
