package binarysearchtrees

import java.util.Scanner
import java.util.Stack

/*
* Tree traversals (in-order, pre-order and post-order)
* WITHOUT recursion! (local stack usage)
*
* */
object TreeOrders {

  case class Node(key: Int, left: Int, right: Int)

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()

    val tree = new Array[Node](n)
    for (i <- 0 until n)
      tree(i) = Node(s.nextInt(), s.nextInt(), s.nextInt())

    traverseInOrder(tree)
    println()
    traversePreOrder(tree)
    println()
    traversePostOrder(tree)
  }

  def traverseInOrder(tree: Array[Node]) = {

    val stack = new Stack[Node]()
    var node = tree(0) // root

    while (node != null || !stack.isEmpty) {

      if (node != null) {
        stack.push(node)
        node = if (node.left != -1) tree(node.left) else null
      }
      else {
        node = stack.pop()
        print(node.key + " ")
        node = if (node.right != -1) tree(node.right) else null
      }
    }
  }

  def traversePreOrder(tree: Array[Node]) = {

    val stack = new Stack[Node]()
    val node = tree(0) // root

    stack.push(node)

    while (!stack.isEmpty) {

      val node = stack.pop()
      print(node.key + " ")

      if (node.right != -1) stack.push(tree(node.right))
      if (node.left != -1) stack.push(tree(node.left))
    }
  }

  def traversePostOrder(tree: Array[Node]) = {

    val stack1 = new Stack[Node]()
    val stack2 = new Stack[Node]()
    val node = tree(0) // root

    stack1.push(node)

    while (!stack1.isEmpty) {

      val tmpNode = stack1.pop()
      stack2.push(tmpNode)

      if (tmpNode.left != -1) stack1.push(tree(tmpNode.left))
      if (tmpNode.right != -1) stack1.push(tree(tmpNode.right))
    }

    while (!stack2.isEmpty) {
      val tmpNode = stack2.pop()
      print(tmpNode.key + " ")
    }

  }

  /*
    5
    4 1 2
    2 3 4
    5 -1 -1
    1 -1 -1
    3 -1 -1

    10
    0 7 2
    10 -1 -1
    20 -1 6
    30 8 9
    40 3 -1
    50 -1 -1
    60 1 -1
    70 5 4
    80 -1 -1
    90 -1 -1
  */

}
