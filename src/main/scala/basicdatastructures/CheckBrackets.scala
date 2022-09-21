package basicdatastructures

import java.util.Scanner
import scala.annotation.tailrec

object CheckBrackets {

  // A stack entry: bracket type and position in input string
  case class StackBracket(bracket: Char, pos: Int)

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val expression = s.next()

    val stack = List[StackBracket]()
    val validation = validate(expression.toList, stack, 1)
    println( if(validation == 0) "Success" else validation)
  }

  @tailrec
  def validate(expression: List[Char], stack: List[StackBracket], pos: Int): Int = {

    expression match {
      case Nil =>
        if (stack.isEmpty) 0 // Success
        else stack.head.pos  // Failure: bracket(s) left on the stack
      case c :: tail if isClosing(c) =>
        if (stack.isEmpty || stack.head.bracket != openingFor(c)) pos // Failure: closing bracket found, but no corresponding opening on the stack
        else validate(tail, stack.tail, pos + 1)  // Continue: remove stack head that matches current closing bracket
      case c :: tail if isOpening(c) =>
        validate(tail, StackBracket(c,pos) +: stack, pos + 1) // Continue: Opening bracket found, put it on the stack
      case _ :: tail =>
        validate(tail, stack, pos + 1) // Continue: skip non-bracket characters
    }
  }

  def isOpening(c: Char): Boolean = Set('(','{','[').contains(c)
  def isClosing(c: Char): Boolean = Set(')','}',']').contains(c)

  def openingFor(c: Char): Char =
    c match {
      case ')' => '('
      case '}' => '{'
      case ']' => '['
      case _ => ' '
    }

}
