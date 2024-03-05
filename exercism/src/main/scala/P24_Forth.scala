import Forth._
import ForthError.{DivisionByZero, ForthError, InvalidWord, StackUnderflow, UnknownWord}

import scala.annotation.tailrec

class P24_Forth {}

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

trait ForthEvaluatorState {
  def toString: String
}

abstract class Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]
}

trait ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]

  def processInput(inputList: List[String]): Either[ForthError, ForthEvaluatorState]
}

class Forth(stack: ForthStack, userDefinedWords: Map[String, List[String]]) extends ForthEvaluator {
  def this() = this(new ForthStack, Map.empty)

  private def performMathOperation(mathOperator: String): Either[ForthError, ForthEvaluator] = {
    val operationResult = if (stack.size < 2) Left(StackUnderflow)
    else {
      val second = stack.peek
      val first = stack.tail.peek
      mathOperator match {
        case MathPlus => Right(first + second)
        case MathMinus => Right(first - second)
        case MathMultiplication => Right(first * second)
        case MathDivision => if (second == 0) Left(DivisionByZero) else Right(first / second)
      }
    }
    operationResult.fold(Left(_), mathResult => Right(new Forth(stack.afterMathOperation.push(mathResult), userDefinedWords)))
  }

  private def performForthOperation(forthOperation: String): Either[ForthError, ForthEvaluator] = {
    val operationResult = if (OneArgForthOperations.contains(forthOperation) && stack.size >= 1) Right(forthOperation match {
      case ForthDup => stack.push(stack.peek)
      case ForthDrop => stack.tail
    })
    else if (TwoArgForthOperations.contains(forthOperation) && stack.size >= 2) Right(forthOperation match {
      case ForthSwap =>
        val top = stack.peek
        val bottom = stack.tail.peek
        stack.tail.tail.push(top).push(bottom)
      case ForthOver => stack.push(stack.tail.peek)
    })
    else Left(StackUnderflow)
    operationResult.fold(Left(_), resultStack => Right(new Forth(resultStack, userDefinedWords)))
  }


  @tailrec
  private def resolveComplexExpression(expressions: List[String], result: List[String]): List[String] =
    expressions.headOption match {
      case None => result.reverse
      case Some(definedWord) =>
        if (userDefinedWords.contains(definedWord))
          resolveComplexExpression(expressions.tail, userDefinedWords(definedWord) ++ result)
        else resolveComplexExpression(expressions.tail, definedWord +: result)
    }

  private def processWordRegistration(inputList: List[String]): Either[ForthError, ForthEvaluator] = {
    val key :: expressions = inputList.takeWhile(DefinitionEndCond)
    val resolvedExpression = resolveComplexExpression(expressions, List.empty)
    if (key.matches(DigitRegex.toString()))
      Left(InvalidWord)
    else Right(new Forth(stack, userDefinedWords.updated(key, resolvedExpression)))
  }

  override def processInput(inputList: List[String]): Either[ForthError, ForthEvaluatorState] = {
    inputList.headOption match {
      case None => Right(stack)
      case Some(input) => input match {
        case registerWord if registerWord == DefinitionStartSymbol =>
          processWordRegistration(inputList.tail).fold(
            Left(_),
            _.processInput(inputList.dropWhile(DefinitionEndCond).tail)
          )
        case wordUse if userDefinedWords.contains(wordUse) =>
          processInput(userDefinedWords(wordUse) ++ inputList.tail)
        case DigitRegex(digit) =>
          new Forth(stack.push(digit.toInt), userDefinedWords).processInput(inputList.tail)
        case mathOperation if MathOperators.contains(mathOperation) =>
          performMathOperation(mathOperation).fold(
            Left(_),
            _.processInput(inputList.tail)
          )
        case forthOperation if ForthOperations.contains(forthOperation) =>
          performForthOperation(forthOperation).fold(
            Left(_),
            _.processInput(inputList.tail)
          )
        case _ => Left(UnknownWord)
      }
    }
  }

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    processInput(text.toLowerCase.split(" ").toList)
  }
}

object Forth {
  private val ForthDup = "dup"
  private val ForthDrop = "drop"
  private val ForthSwap = "swap"
  private val ForthOver = "over"
  private val OneArgForthOperations = List(ForthDup, ForthDrop)
  private val TwoArgForthOperations = List(ForthSwap, ForthOver)
  private val ForthOperations = OneArgForthOperations ++ TwoArgForthOperations
  private val DigitRegex = "(\\d+)".r
  private val MathPlus = "+"
  private val MathMinus = "-"
  private val MathMultiplication = "*"
  private val MathDivision = "/"
  private val MathOperators = List(MathPlus, MathMinus, MathMultiplication, MathDivision)
  private val DefinitionEndCond: String => Boolean = _ != ";"
  private val DefinitionStartSymbol = ":"

}

class ForthStack(stackList: List[Int]) extends ForthEvaluatorState {
  def this() = this(List.empty[Int])

  def push(value: Int): ForthStack = new ForthStack(value +: stackList)

  def peek: Int = stackList.head

  def tail: ForthStack = new ForthStack(stackList.tail)

  def size: Int = stackList.size

  def afterMathOperation: ForthStack = this.tail.tail

  override def toString: String = stackList.reverse.mkString(" ")
}


