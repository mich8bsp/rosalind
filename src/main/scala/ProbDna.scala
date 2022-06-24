import Rosalind._

object ProbDna {

  def solve(data: List[String]): Any = {
    def countNucleotide(str: String)(nuc: Char): Int = str.count(_ == nuc)

    data.foldLeft(DnaNucleotides.map(_ => 0)) {
      case (currCount, currLine) =>
        val countInLine = countNucleotide(currLine)_

        currCount.zipWithIndex.map {
          case (currCountForNucleotide, idx) =>
            currCountForNucleotide + countInLine(DnaNucleotides(idx))
        }
    } mkString " "
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_dna.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
