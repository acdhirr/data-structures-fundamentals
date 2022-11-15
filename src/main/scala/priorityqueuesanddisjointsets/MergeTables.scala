package priorityqueuesanddisjointsets

import java.util.Scanner

object MergeTables {

  class Table(var rows: Int) {

    private var _parent: Table = this
    var rank = 0

    // heuristic path to parent compression: shift node up node to root parent
    def parent: Table =  {
      while (_parent._parent != _parent) _parent = _parent._parent
      _parent
    }

    def setParent(table: Table) = _parent = table.parent
  }

  class MergeQuery(val dest: Int, val src: Int)

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val m = s.nextInt()
    val tables = new Array[Table](n)
    val queries = new Array[MergeQuery](m)

    for (i <- 0 until n )
      tables(i) = new Table(s.nextInt())

    // initial max value
    var max = tables.maxBy(_.rows).rows

    for (i <- 0 until m)
      queries(i) = new MergeQuery(s.nextInt(), s.nextInt())

    for (i <- 0 until m) {
      // max is recalculated after each query
      max = runQuery(tables,queries(i),max)
      println(max)
    }

  }

  def runQuery(tables: Array[Table], q: MergeQuery, max: Int): Int = {

    val dest: Table = tables(q.dest-1).parent
    val src: Table = tables(q.src-1).parent  // array is 0-based

    // If nothing changes:
    if (src == dest) max
    // The destination tree has a higher rank, so make it the parent of source
    else if (dest.rank > src.rank) {
      dest.rows += src.rows
      src.setParent(dest)
      src.rows = 0
      Set(max, dest.rows).max
    }
    // The source tree has equal or higher rank, so make it the destination
    else {
      src.rows += dest.rows
      dest.setParent(src)
      dest.rows = 0
      if (dest.rank == src.rank) src.rank += 1  // (see drawing below)
      Set(max, src.rows).max
    }

    /*
         dest     src
                  O
           O_____/ \
          /         O
         O           \
        /             O
       O

       - src rank is equal to or larger than dest rank
       - only when it's equal, attaching dest to src will result in a higher rank for src
    */

  }

}

/*

5 5
1 1 1 1 1
3 5
2 4
1 4
5 4
5 3

6 4
10 0 5 0 3 3
6 6
6 5
5 4
4 3

*/