import Rosalind._

object ProbRna {

  def solve(data: List[String]): Any = {
    data.mkString("").map(nuc => DnaToRna(nuc))
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_rna.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
