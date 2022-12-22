import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

    // My code
    def f(num: Double, i:Int = 9): Double = {
        if (i == 0) 1
        else f(num,i-1) + math.pow(num,i) / factorial(i)
    } 
    
    def factorial(i:Int): Int = {
        if (i==0) 1
        else factorial(i-1) * i
    }
    // End of my code

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val n = stdin.readLine.trim.toInt

        for (nItr <- 1 to n) {
            val x = stdin.readLine.trim.toDouble
            // My code
            println(f(x))
            // End of my code
        }
    }
}
