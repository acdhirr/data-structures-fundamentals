package hashtablesandhashfunctions

import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Find all occurrences of a pattern in a string
 * using hashing and the Rabin-Karp algorithm
 */
object HashSubstring {

  val Prime = 10000019 // 7963 1000003 10000019
  val HashX = 2

  /*
    world
    eeviuhrfhruhifhohfehoiworldfhiorhfirheworldoifhirfefd
  */

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val pattern = s.next()
    val text = s.next()

    /* test
    val file = scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("text3.txt"))
    val text = file.mkString
    val pattern = text.substring(0,20000)
    */

    // val t1 = System.nanoTime
    val found = find(pattern, text)
    // val dt1 = (System.nanoTime - t1) / 1e9d

    if (!found.isEmpty) found.foreach(s => print(s"$s "))

    //println()
    //println(dt1)
  }

  def find(pattern: String, text: String): ListBuffer[Int] = {

    val hits = ListBuffer[Int]()

    val patternHash = hash(pattern)
    val pLength = pattern.length
    val tLength = text.length

    val pow = modPower(HashX,pLength,Prime)

    /* Search text from end (minus pattern length) to start
       Rolling hashes are computed while looping,
       so without pre-storing them in an array
    */
    @tailrec
    def _find(rollingHash: Int, i: Int): Unit = {

      if (rollingHash.equals(patternHash)
         && pattern.equals(text.substring(i, i + pLength)) // filter out false positives
      )
          hits.prepend(i)

      if (i > 0) {
        /* Compute the hash for a string pattern(0..n-1)
           from the given hash for string pattern(1..n)

           in:  hash for            bcdefgh
           in:  pattern            abcdefgh
           out: leftRoll hash for  abcdefg
        */
        val h = mod(HashX * rollingHash + text(i-1) - text(i+pLength-1) * pow, Prime) // always > 0

        _find(h, i-1)
      }
    }

    if (pLength <= tLength) {

      val startIndex = tLength - pLength
      val startSubs = text.substring(startIndex, startIndex + pLength)
      val startHash = hash(startSubs)

      // start rolling
      _find(startHash, startIndex )
    }

    hits
  }

  // See HashChains.scala
  def hash(s: String): Int = s.foldRight(0)((c, acc) => (c + acc * HashX) % Prime)

  // Modulo function that never goes below zero
  def mod(a: Int, b: Int) = ((a % b) + b) % b

  // Recursively calculates base^exp % mod
  def modPower(base: Int, exp: Int, mod: Int): Int = {

    @tailrec
    def _power(result: Int, exp: Int): Int = exp match {
      case 0 => result
      case _ => _power((result * base) % mod, exp - 1)
    }

    _power(1, exp)
  }

}

