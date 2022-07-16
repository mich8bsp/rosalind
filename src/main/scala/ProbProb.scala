import Rosalind._

object ProbProb {

  private def prob(gcContent: Double, dna: String): Double = {
    val nucleotideProbs: Map[Char, Double] = Map(
      'C' -> gcContent/2,
      'G' -> gcContent/2,
      'T' -> (1D-gcContent)/2,
      'A' -> (1D-gcContent)/2
    )

    val totalProb: Double = dna.foldLeft(1D) {
      case (currProb, currNucleotide) =>
        currProb * nucleotideProbs(currNucleotide)
    }

    math.log10(totalProb)
  }

  def solve(data: List[String]): Any = {
    val dna = data.head
    val gcContentArr: Array[Double] = data(1).split(" ").map(_.toDouble)
    gcContentArr.map(prob(_, dna)).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_prob.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
