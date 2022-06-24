import Rosalind._

object ProbMrna {

  val AminoAcidToRna = readFile("RnaToAminoAcidsTable.txt")
    .flatMap(_.split("  "))
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(x => {
      val Array(rna, aminoAcid) = x.split(" ")
      aminoAcid -> rna
    })
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2))
    .toMap

  def solve(data: List[String]): Any = {
    val aminoAcids: List[String] = data.mkString.toList.map(_.toString) :+ "Stop"
    aminoAcids.foldLeft(1) {
      case (numOfRna, curr) => (numOfRna * AminoAcidToRna(curr).size) % 1000000
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_mrna.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
