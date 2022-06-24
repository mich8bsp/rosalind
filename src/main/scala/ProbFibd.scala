import Rosalind._

import scala.collection.mutable

object ProbFibd {

  def solve(data: List[String]): Any = {
    val Array(n, m) = data.head.split(" ")
      .map(_.toInt)

    val cacheNewborn: mutable.Map[Int, Long] = mutable.Map.empty[Int, Long]
    val cacheMature: mutable.Map[Int, Long] = mutable.Map.empty[Int, Long]

    def newbornes(i: Int) = cacheNewborn.getOrElseUpdate(i, fibNewBorn(i))
    def mature(i: Int) = cacheMature.getOrElseUpdate(i, fibMature(i))

    def fibMature(i: Int): Long = i match {
      case x if x <= 1 => 0
      case _ =>
        val matureFromPrevMonth = mature(i - 1)
        val newbornsFromPrevMonth = newbornes(i - 1)
        val dead = newbornes(i - m)
        matureFromPrevMonth + newbornsFromPrevMonth - dead
    }

    def fibNewBorn(i: Int): Long = i match {
      case 1 => 1
      case x if x < 1 => 0
      case _ => mature(i - 1)
    }

    mature(n) + newbornes(n)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_fibd.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
