package hashtablesandhashfunctions

import java.util.Scanner
import java.util.LinkedList

object HashChains {

  sealed abstract class Query
  case class Add(word: String) extends Query
  case class Delete(word: String) extends Query
  case class Find(word: String) extends Query
  case class Check(row: Int) extends Query

  class Table(size: Int) {

    val buckets = new Array[LinkedList[String]](size)
    for (i <- 0 until size) buckets(i) = new LinkedList[String]()

    def delete(word: String): Unit =
      buckets( hashFast(word, size) ).removeIf(_ == word)

    def add(word: String): Unit = {
      val lst = buckets( hashFast(word, size) )
      if (!lst.contains(word)) lst.addFirst(word)
    }

    def find(word: String): Boolean =
      buckets( hashFast(word, size) ).contains(word)

    def check(row: Int): LinkedList[String] = {
      buckets(row)
    }
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val buckets = s.nextInt() // size of hash table
    val table = new Table(buckets)
    val queryCount = s.nextInt()
    val queries = new Array[Query](queryCount)

    for (i <- 0 until queryCount) {

      val query = s.next()
      queries(i) = query match {
        case "add"   => Add(s.next())
        case "del"   => Delete(s.next())
        case "find"  => Find(s.next())
        case "check" => Check(s.nextInt())
      }
    }

    process(queries, table)
  }

  def process(queries: Array[Query], table: Table) = {

    queries.foreach(q => {

      q match {
        case Add(w)    => table.add(w)
        case Delete(w) => table.delete(w)
        case Find(w)   => if (table.find(w)) println("yes") else println("no")
        case Check(i)  => {
          val p = table.check(i).toArray.toList.mkString(" ")
          println(p)
        }
      }
    })
  }

  /*
                 |S|-1
       h(S) = ((   ∑︁ S(i).x^i ) mod p ) mod m
                  i=0

       m = buckets
       p = prime
       S = String to be hashed (the hashee)
  */
  def hash(s: String, buckets: Int): Int = {

    val prime = 1000000007L
    val x = 263L
    val h = s.zipWithIndex.foldLeft(0L){ case (acc,(c,i)) => (c * math.pow(x,i).toLong + acc) % prime } % buckets
    h.toInt
  }

  /*
      ∑︁ S(i).x^i

      7 + 2x + 5x² + 3x³
                  <----| foldRight

      c  | acc              acc*x + c
      ------------------------------------------
      3  | 0                3
      5  | 3                3x  + 5
      2  | 3x  + 5          3x² + 5x  + 2
      7  | 3x² + 5x + 2     3x³ + 5x² + 2x + 7
  */
  def hashFast(s: String, buckets: Long): Int = {

    val prime = 1000000007
    val x = 263
    val h =s.foldRight(0L)( (c, acc) => (c + acc*x) % prime) % buckets
    h.toInt
  }


}
