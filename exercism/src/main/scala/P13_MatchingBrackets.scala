import scala.annotation.tailrec
import scala.collection.mutable

class P13_MatchingBrackets {}

object MatchingBrackets {
  def isPaired(brackets: String): Boolean = {
    val mapping = Map(
      '}' -> '{',
      ']' -> '[',
      ')' -> '('
    )
    val stack = mutable.Stack[Char]()

    @tailrec
    def lookForPair(list: List[Char]): Boolean = list match {
      case Nil if stack.isEmpty => true
      case Nil if stack.nonEmpty => false
      case x :: xs => x match {
        case c if Seq('{', '[', '(') contains c =>
          stack.push(c)
          lookForPair(xs)
        case c if Seq('}', ']', ')').contains(c) && stack.nonEmpty && stack.top == mapping(c) =>
          stack.pop()
          lookForPair(xs)
        case c if Seq('}', ']', ')').contains(c) => false
        case _ => lookForPair(xs)
      }
    }

    lookForPair(brackets.toList)
  }

}

