package binarysearchtrees

import java.util.Scanner
import java.util.Stack
import scala.collection.mutable.ListBuffer

object IsBSTHard {

  case class Node(key: Int, left: Int, right: Int) {

    override def toString: String =  left + " " + key + " " + right
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()

    val tree = new Array[Node](n)
    for (i <- 0 until n)
      tree(i) = Node(s.nextInt(), s.nextInt(), s.nextInt())

    // Put ordered nodes in a list
    val r = traverseInOrder(tree)

    // List values must be increasing continuously for a BST to be valid
    if (isAscending(r, tree)) println("CORRECT")
    else println("INCORRECT")

  }

  def isAscending(lst: ListBuffer[Node], tree: Array[Node]): Boolean = {

    if (!lst.isEmpty) lst.reduce((a,b) => {

      if (b.key < a.key) return false
      // if a and b have the same value, a must not be the left child of b!
      if (b.key == a.key && b.left != -1 && a == tree(b.left)) return false
      b
    })
    true
  }


  def traverseInOrder(tree: Array[Node]) = {

    val ordered = ListBuffer[Node]()
    val stack = new Stack[Node]()
    var node = if (!tree.isEmpty) tree(0) else null // root

    while (node != null || !stack.isEmpty) {

      if (node != null) {
        stack.push(node)
        node = if (node.left != -1) tree(node.left) else null
      }
      else {
        node = stack.pop()
        ordered.append(node)
        node = if (node.right != -1) tree(node.right) else null
      }
    }

    ordered
  }

  /*

  3
  2 1 2
  1 -1 -1
  3 -1 -1
  -> CORRECT

  3
  1 1 2
  2 -1 -1
  3 -1 -1
  -> INCORRECT

  3
  2 1 2
  1 -1 -1
  2 -1 -1
  -> CORRECT

  3
  2 1 2
  2 -1 -1
  3 -1 -1
  -> INCORRECT

  1
  2147483647 -1 -1
  -> CORRECT

  5
  1 -1 1
  2 -1 2
  3 -1 3
  4 -1 4
  5 -1 -1
  -> CORRECT

  7
  4 1 2
  2 3 4
  6 5 6
  1 -1 -1
  3 -1 -1
  5 -1 -1
  7 -1 -1
  -> CORRECT

  */

}
