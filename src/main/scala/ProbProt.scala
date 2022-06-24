import Rosalind._

object ProbProt {

  def solve(data: List[String]): Any = {
    data.mkString.grouped(3)
      .map(RnaToAminoAcid(_))
      .takeWhile(_ != "Stop")
      .mkString
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_prot.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
