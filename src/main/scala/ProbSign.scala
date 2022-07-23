import Rosalind._

object ProbSign {

  def solve(data: List[String]): Any = {
    val n = data.head.toInt

    def signedPermutations(permutations: List[List[Int]], elementsAvailable: Set[Int]): List[List[Int]] = {
      if (elementsAvailable.isEmpty) {
        permutations
      } else {
        elementsAvailable.toList.flatMap(element => {
          signedPermutations(permutations.map(_ :+ element), elementsAvailable - element) ++
            signedPermutations(permutations.map(_ :+ (-element)), elementsAvailable - element)
        })
      }
    }

    val permutations = signedPermutations(List(Nil), (1 to n).toSet)

    println(permutations.size)
    permutations.map(_.mkString(" ")).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_sign.txt"
    val data = readFile(filename)
    solve(data)
  }
}
