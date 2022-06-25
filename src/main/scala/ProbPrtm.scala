import Rosalind._

object ProbPrtm {

  def solve(data: List[String]): Any = {
    data.mkString
      .map(aminoAcid => AminoAcidToWeight(aminoAcid.toString))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_prtm.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
