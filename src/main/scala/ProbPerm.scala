import Rosalind._

object ProbPerm {

  def solve(data: List[String]): Any = {
    val n = data.head.toInt

    def permutations(i: Int): List[List[Int]] = {
      if (i == 1) {
        List(List(i))
      } else {
        permutations(i - 1).flatMap(subPerm => {
          subPerm.indices.map(idxOfInsertion => {
            (subPerm.slice(0, idxOfInsertion) :+ i) ::: subPerm.slice(idxOfInsertion, subPerm.length)
          }).toList :+ (subPerm :+ i)
        })
      }
    }

    val res = permutations(n)
    println(res.length)
    res.map(_.mkString(" "))
      .foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_perm.txt"
    val data = readFile(filename)
    solve(data)
  }
}
