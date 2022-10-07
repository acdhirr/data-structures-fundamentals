package priorityqueuesanddisjointsets

import java.util.Scanner
import scala.collection.mutable.PriorityQueue

/*

4 20
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

2 5
1 2 3 4 5

*/

object JobQueue {

  case class ScheduledJob(thread: Int, start: Long )

  case class Thread(id: Int, freeAt: Long) extends Ordered[Thread] {

    // Threads with earliest free time are favoured.
    // For equal threads those with the lower id are favoured.
    override def compare(that: Thread): Int = {
      if (this.freeAt == that.freeAt) that.id - this.id
      else (that.freeAt - this.freeAt).toInt
    }
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val threadCount = s.nextInt()
    val n = s.nextInt()
    val jobs = new Array[Long](n)
    for (i <- 0 until n) jobs(i) = s.nextLong()

    val result = schedule(threadCount, jobs)

    // print thread ids and start times
    result.foreach(job => println(s"${job.thread} ${job.start}"))
  }

  /*
    For an array of jobs (a job is only described by its start time)
    and a fixed number (threadCount) of threads return an array of ScheduledJobs
  */
  def schedule(threadCount: Int, jobs: Array[Long] ): Array[ScheduledJob] = {

    val scheduledJobs = new Array[ScheduledJob](jobs.length)
    val threads = PriorityQueue[Thread]()

    // Put all threadCount threads in a priority tree
    for(i <- 0 until threadCount) threads.enqueue(Thread(i,0))

    // assign threads to all jobs
    for (i <- 0 until jobs.length) {
      // the head (root) of the priority tree contains the first available thread
      val first = threads.dequeue()
      scheduledJobs(i) = ScheduledJob( first.id, first.freeAt )
      // the assigned (dequeued thread will be available again after jobs(i) seconds)
      // so enqueue it with its new availability time
      threads.enqueue( Thread(first.id, first.freeAt + jobs(i)) )
    }

    scheduledJobs
  }

}
