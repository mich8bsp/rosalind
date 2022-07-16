import Rosalind._

import scala.collection.mutable

object ProbLgis {

  def solve(data: List[String]): Any = {
    val n = data.head.toInt
    val permutation = data(1).split(" ").map(_.toInt).toList

    def lis(ordering: Ordering[Int]) = {
      val cache: Array[List[Int]] = permutation.map(List(_)).toArray
      (1 until permutation.length).foreach(i => {
        (0 until i).foreach(j => {
          if (ordering.gt(permutation(i), permutation(j)) && cache(i).size < cache(j).size + 1) {
            cache(i) =  cache(j) :+ permutation(i)
          }
        })
      })

      cache.maxBy(_.length)
    }


    println(lis(Ordering[Int]).mkString(" "))
    println(lis(Ordering[Int].reverse).mkString(" "))
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_lgis.txt"
    val data = readFile(filename)
    solve(data)
  }
}
