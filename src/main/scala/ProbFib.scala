import Rosalind._

import scala.collection.mutable

object ProbFib {

  def solve(data: List[String]): Any = {
    val Array(n, k) = data.head.split(" ")
      .map(_.toInt)

    val cache: mutable.Map[Int, Long] = mutable.Map.empty[Int, Long]

    def fib(i: Int): Long = i match {
      case 1 | 2 => 1L
      case _ =>
        val prevMonth: Long = cache.getOrElseUpdate(i - 1, fib(i - 1))
        val offsprings: Long = cache.getOrElseUpdate(i - 2, fib(i - 2)) * k

        prevMonth + offsprings
    }

    fib(n)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_fib.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
