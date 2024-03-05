class P24_Forth {

}

import Forth._
import ForthError.{DivisionByZero, ForthError, InvalidWord, StackUnderflow}

import scala.annotation.tailrec

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

trait ForthEvaluatorState {
  // TODO: Implement. return the current stack as Text with the element
  // on top of the stack being the rightmost element in the output."
  def toString: String
}

abstract class Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]
}

trait ForthEvaluator {
  // TODO: Implement evaluation
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

class Forth extends ForthEvaluator {
  // TODO Use this.stack!
  private val stack: ForthStack = new ForthStack()

  private def performMathOperation(mathOperator: String, stack: ForthStack): Either[ForthError, Int] = {
    if (stack.size < 2) Left(StackUnderflow)
    else {
      val second = stack.peek
      val first = stack.tail.peek
      mathOperator match {
        case "+" => Right(first + second)
        case "-" => Right(first - second)
        case "*" => Right(first * second)
        case "/" => if (second == 0) Left(DivisionByZero) else Right(first / second)
      }
    }
  }

  private def performForthOperation(forthOperation: String, stack: ForthStack): Either[ForthError, ForthStack] =
    if (OneArgForthOperations.contains(forthOperation) && stack.size >= 1) Right(forthOperation match {
      case "dup" => stack.push(stack.peek)
      case "drop" => stack.tail
    })
    else if (TwoArgForthOperations.contains(forthOperation) && stack.size >= 2) Right(forthOperation match {
      case "swap" =>
        val top = stack.peek
        val bottom = stack.tail.peek
        stack.tail.tail.push(top).push(bottom)
      case "over" => stack.push(stack.tail.peek)
    })
    else Left(StackUnderflow)


  @tailrec
  private def normalizeExpression(expression: List[String], userDefinedWords: Map[String, List[String]], result: List[String]): List[String] =
    expression.headOption match {
      case None => result.reverse
      case Some(x) => if (userDefinedWords.contains(x)) normalizeExpression(expression.tail, userDefinedWords, userDefinedWords(x) ++ result)
      else normalizeExpression(expression.tail, userDefinedWords, x +: result)
    }

  private def processWordRegistration(inputList: List[String], userDefinedWords: Map[String, List[String]]): Either[ForthError, Map[String, List[String]]] = {
    val key :: expression = inputList.takeWhile(_ != ";")
    val newExpression = normalizeExpression(expression, userDefinedWords, List.empty)
    if (key.matches("\\d+")) {
      Left(InvalidWord)
    }
    else Right(userDefinedWords.updated(key, newExpression))
  }

  private def processInput(inputList: List[String], stack: ForthStack, userDefinedWords: Map[String, List[String]]): Either[ForthError, ForthEvaluatorState] = {
    inputList.headOption match {
      case None => Right(stack)
      case Some(input) => input match {
        // register function
        case registerWord if registerWord == ":" =>
          processWordRegistration(inputList.tail, userDefinedWords).fold(
            Left(_),
            newUserDefinedWords => processInput(inputList.dropWhile(_ != ";").tail, stack, newUserDefinedWords)
          )
        // compute function value
        case wordUse if userDefinedWords.contains(wordUse) => processInput(userDefinedWords(wordUse) ++ inputList.tail, stack, userDefinedWords)
        case DigitRegex(digit) => processInput(inputList.tail, stack.push(digit.toInt), userDefinedWords)
        case mathOperation if MathOperators.contains(mathOperation) =>
          performMathOperation(mathOperation, stack).fold(
            Left(_),
            result => processInput(inputList.tail, stack.afterMathOperation.push(result), userDefinedWords)
          )
        case forthOperation if ForthOperations.contains(forthOperation) =>
          performForthOperation(forthOperation, stack).fold(
            Left(_),
            resultStack => processInput(inputList.tail, resultStack, userDefinedWords)
          )
        case _ => Left(InvalidWord)
      }
    }
  }

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    processInput(text.toLowerCase.split(" ").toList, stack, Map.empty)
  }
}

object Forth {
  private val DigitRegex = "(\\d+)".r
  private val MathOperators = List("+", "-", "*", "/")
  private val ForthOperations = List("dup", "drop", "swap", "over")
  private val OneArgForthOperations = ForthOperations.take(2)
  private val TwoArgForthOperations = ForthOperations.drop(2)
}

class ForthStack(stack: List[Int]) extends ForthEvaluatorState {
  def this() = this(List.empty[Int])

  def push(value: Int): ForthStack = new ForthStack(stack.prepended(value))

  def peek: Int = stack.head

  def tail: ForthStack = new ForthStack(stack.tail)

  def size: Int = stack.size

  def afterMathOperation: ForthStack = this.tail.tail

  override def toString: String = stack.reverse.mkString(" ")
}


