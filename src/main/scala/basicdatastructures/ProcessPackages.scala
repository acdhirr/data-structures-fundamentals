package basicdatastructures

import java.util.Scanner
import scala.collection.mutable

object ProcessPackages {

  case class Package(time: Int, duration: Int, startTime: Int) {
    val readyTime = startTime + duration
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val bufferSize = s.nextInt()
    val packageCount = s.nextInt()
    val lines = new Array[(Int,Int)](packageCount)
    for (i <- 0 until packageCount) lines(i) = (s.nextInt(), s.nextInt())

    process(lines, bufferSize)
  }


  def process(lines: Array[(Int,Int)], bufferSize: Int): Unit = {

    val queue = new mutable.Queue[Package]()
    val n = lines.length

    for (i <- 0 until n) {

      val curLine = lines(i)
      val time = curLine._1
      val duration = curLine._2
      val startTime = if (!queue.isEmpty) Math.max(queue.last.readyTime, time) else time
      val pNext = Package(time, duration, startTime)

      // remove packages that have been processed at pNext.time
      while (!queue.isEmpty && queue.head.readyTime <= pNext.time) queue.dequeue()

      if (queue.size < bufferSize) {
        queue.enqueue(pNext)
        println(pNext.startTime)
      }
      else println(-1) // package rejected due to buffer overload
    }

  }

  /**
   2 4
   0 3
   0 5
   0 1
   0 7

   ->

   0
   3
   -1
   -1
  */
  import java.util.Scanner
  import scala.collection.mutable

  object ProcessPackages {

    case class Package(time: Int, duration: Int, startTime: Int) {
      val readyTime = startTime + duration
    }

    def main(args: Array[String]) = {

      val s = new Scanner(System.in)
      val bufferSize = s.nextInt()
      val packageCount = s.nextInt()
      val lines = new Array[(Int,Int)](packageCount)
      for (i <- 0 until packageCount) lines(i) = (s.nextInt(), s.nextInt())

      process(lines, bufferSize)
    }


    def process(lines: Array[(Int,Int)], bufferSize: Int): Unit = {

      val queue = new mutable.Queue[Package]()
      val n = lines.length

      for (i <- 0 until n) {

        val curLine = lines(i)
        val time = curLine._1
        val duration = curLine._2
        val startTime = if (!queue.isEmpty) Math.max(queue.last.readyTime, time) else time
        val pNext = Package(time, duration, startTime)

        // remove packages that have been processed at pNext.time
        while (!queue.isEmpty && queue.head.readyTime <= pNext.time) queue.dequeue()

        if (queue.size < bufferSize) {
          queue.enqueue(pNext)
          println(pNext.startTime)
        }
        else println(-1) // package rejected due to buffer overload
      }

    }

    /**
    2 4
   0 3
   0 5
   0 1
   0 7

   ->

   0
   3
   -1
   -1
     */


  }

}

