import scala.io.StdIn.readLine
import scala.math
import java.lang.Math
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.time.Year

object P01_Elementary {

    def P1HelloWorld : Unit = println("Hello World")

    def P2AskAndGreet : Unit = {
        println("What is Your name?")
        println("Hello " + readLine() + "!")
    }

    def P3AliceAndBob : Unit = {
        println("What is Your name?")
        val name = readLine()
        if (name.equals("Alice") || name.equals("Bob"))
            println("Hello " + name + "!")
        else
            println("Hello!")
    }

    def P4SumToN : Unit = {
        print("Enter a number: ")
        val n = readLine()
        println("Sum of numbers 1 to " + n + " is: " + List.range(1,n.toInt).sum)
    }

    def P5Multiples5And3 : Unit = {
        print("Enter a number: ")
        var (sum, i, n) = (0, 0, readLine())
        for (i <- 1 to n.toInt) {
            if (i%3 == 0 || i%5 == 0) sum += i
        }
        println("Sum of x%3 and x%5 numbers 1 to " + n + " is: " + List.range(1,n.toInt).sum)
    }

    def P6SumOrProduct : Unit = {
        print("Enter a number: ")
        val n = readLine()
        println("What do You want to compute?\n" + 
        "Enter S for sum of numbers 1-"+n+"\n"+
        "Enter P for product of numbers 1-"+n)
        val input = readLine()
        if (input.equals("S")) println("Sum of 1-"+n+" is: "+ List.range(1,n.toInt).sum)
        else if (input.equals("P")) println("Product of 1-"+n+" is: "+List.range(1,n.toInt).product)
    }

    def P7MultiplicationTable : Unit = {
        var (i,j) = (0,0)
        for (i <- 1 to 12){
            for (j <- 1 to 12){
                printf("%3d ", i*j)
            }
            println()
        }
    }

    def P8PrimeNumbers : Unit = {
        try {
            var num = 2
            var tmp = 2
            var break = false
            while (true){
                tmp = 2
                break = false
                while (tmp <= math.sqrt(num).toInt && !break){
                    if (num % tmp == 0) {break = true}
                    tmp += 1
                }
                if (!break) println(num)
                num = Math.addExact(num, 1)
            }
        } catch {
            case ex: java.lang.ArithmeticException => println("Integer's bound reached")
        }
    }

    def P9GuessingGame : Unit = {
        val numberToGuess = Random.nextInt()
        var numberOfTries = 0
        var triedNumbers = ArrayBuffer[Int]()
        var guessedNumber = 0
        var game = true
        while(game){
            print("Guess the secret number:")
            guessedNumber = readLine().toInt
            numberOfTries += 1
            if (guessedNumber == numberToGuess){
                println(f"The number was: ${numberToGuess} and You guessed it in ${numberOfTries} tries!")
                game = false
            }
            else{
                if (triedNumbers.contains(guessedNumber)){
                    print("You already guessed this number. ")
                }
                if (guessedNumber < numberToGuess) println("Enter a greater number")
                else println("Enter a lesser number")
                triedNumbers += guessedNumber
            }
        }
    }

    def P10Next10Leapyears : Unit = {
        var year = Year.now.getValue
        var counter = 0
        while(counter < 20){
            if (year%4 == 0 && year%100 != 0 || year%400 == 0){
                println(year)
                counter += 1
            }
            year += 1
        }

    }

    def P11Expression : Unit = {
        var result = 0.0
        for(k <- 1 to 1_000_000){
            result += (math.pow(-1,k+1)/(2*k - 1)).toDouble
        }
        println(f"Result of computation is equal: ${4*result}")
    }

    def main(args: Array[String]) : Unit = {}
}