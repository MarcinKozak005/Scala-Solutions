package P01_Elementary

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import scala.util.Random

object P09GuessingGame extends App {
  def solution1(): Unit = {
    val numberToGuess = Random.nextInt()
    var numberOfTries = 0
    val triedNumbers = ArrayBuffer[Int]()
    var guessedNumber = 0
    var game = true
    while (game) {
      print("Guess the number:")
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

  def solution2(): Unit = {
    val numberToGuess = Random.nextInt()

    @tailrec
    def game(guessedNumbers: List[Int], numberOfTries: Int = 1): Unit = {
      print("Guess the number:")
      readLine().toInt match {
        case input if input == numberToGuess => println(f"The number was: $numberToGuess and You guessed it in $numberOfTries tries!")
        case input if guessedNumbers contains input =>
          print("You already guessed this number. ")
          game(input +: guessedNumbers, numberOfTries)
        case input =>
          println(s"Enter a ${if (input < numberToGuess) "greater" else "lesser"} number")
          game(input +: guessedNumbers, numberOfTries + 1)
      }
    }

    game(List())
  }

  solution1()
  solution2()
}
