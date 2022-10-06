package priorityqueuesanddisjointsets

import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object BuildHeap {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 until n) a(i) = s.nextInt()

    val swaps = buildMinHeap(a)
    println(swaps.size)
    swaps.foreach(s => println(s"${s._1} ${s._2}"))
  }

  def buildMinHeap(a: Array[Int]): List[(Int,Int)] = {

    // Use a list to remember every swap
    // Not needed for the heap building but we need this as output
    val swapList = new ListBuffer[(Int,Int)]()

    // sift down half the array, because the last row (n/2 nodes) contains only leaves
    for (i <- a.length/2 to 0 by -1)
      siftDown(a,i,swapList)

    swapList.toList
  }

  // zero based
  def leftIndex(i: Int) = 2*i+1
  def rightIndex(i: Int) = 2*i+2
  // check if i is within the array
  def indexExists(a: Array[Int], i: Int) = i < a.length

  def swap(a: Array[Int], i: Int, j: Int): Array[Int] = {
    val k = a(j)
    a(j) = a(i)
    a(i) = k
    a
  }

  @tailrec
  def siftDown(a: Array[Int], i: Int, swapList: ListBuffer[(Int,Int)] ): ListBuffer[(Int,Int)] = {

    var minIndex = i
    val left = leftIndex(i)
    val right = rightIndex(i)

    if (indexExists(a,left) && a(left) < a(i)) minIndex = left
    if (indexExists(a,right) && a(right) < a(minIndex)) minIndex = right

    if (minIndex != i) {
      swap(a,i,minIndex)
      swapList.append((i,minIndex))
      // recursive
      siftDown(a, minIndex, swapList )
    }
    else swapList
  }


}

