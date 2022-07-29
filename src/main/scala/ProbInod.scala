import Rosalind._

object ProbInod {

  def solve(data: List[String]): Any = {
    val n = data.head.toInt

    // by induction: start is 3 leaves with 1 internal   o-o<=8
    // for graph with with n-1 internal nodes and n+1 leaves we can get n internal nodes by turning a leaf into internal
    // by connecting it to 2 new leaves, making n internal nodes and n+2 leaves (added 2 removed 1)
    n-2
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_inod.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
