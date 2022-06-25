import Rosalind._

object ProbSplc {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)
    fasta.map(_.dna) match {
      case dna :: introns =>
        val exons = introns.foldLeft(dna) {
          case (currDna, intron) => currDna.replaceAll(intron, "")
        }

        exons.grouped(3)
        .map(codon => RnaToAminoAcid(codon.map(DnaToRna(_))))
        .takeWhile(_ != "Stop")
        .mkString
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_splc.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
