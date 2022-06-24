import Rosalind._

object ProbRevc {

  def solve(data: List[String]): Any = {
    data.mkString("")
      .reverse
      .map(nuc => DnaComplements(nuc))
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_revc.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
