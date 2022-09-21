package basicdatastructures

import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.MutableList

/*
  5
  4 -1 4 1 1
  -> 3

  5
  -1 0 4 0 3
  -> 4
*/


object TreeHeight {

  case class Node(label: Int, parent: Int, var children: MutableList[Node]) {
    override def toString() = s"[$label]-$children"
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val nodes = new Array[Node](n)
    for (i <- 0 until n) nodes(i) = Node(i,s.nextInt(), MutableList.empty)

    nodes.filter(_.parent != -1).foreach(n => nodes(n.parent).children += n)
    val root: Node = nodes.find(_.parent == -1).get

    println(depth(root))
  }

  def depth(node: Node): Int = {

    var depth = 0
    val queue = new mutable.Queue[Node]()

    queue.enqueue(node) // add root to queue

    // breadth first traversal
    while (!queue.isEmpty) {

      val width = queue.size // root = 1
      depth += 1 // start a new level

      // handle all nodes on this level
      for (_ <- 0 until width) {
        val node = queue.dequeue()
        if (!node.children.isEmpty)
          for (child <- node.children) queue.enqueue(child)
      }
    }

    depth
  }
}

