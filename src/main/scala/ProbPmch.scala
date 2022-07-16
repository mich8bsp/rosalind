import Rosalind._

object ProbPmch {

  private def fact(n: Int): Double = {
    var agg: Double = 1D
    var i: Int = 1
    while (i <= n) {
      agg *= i
      i += 1
    }
    agg
  }

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)
    val dna = fasta.map(_.dna).mkString
    val numOfA: Int = dna.count(_ == 'A')
    val numOfC: Int = dna.count(_ == 'C')

    val res: Double = fact(numOfA) * fact(numOfC)
    res
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_pmch.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
