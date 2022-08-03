import Rosalind._

object ProbPdst {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data).map(_.dna).zipWithIndex

    def calcDistance(str1: String, str2: String): Double = str1.zip(str2).map {
        case (c1, c2) if c1 != c2 => 1
        case _ => 0
      }.sum.toDouble / str1.length

    val distanceMatrix: Array[Array[Double]] = Array.fill(fasta.size, fasta.size)(0)
    for {
      (str1, i) <- fasta
      (str2, j) <- fasta if j > i
    } yield {
      val distance = calcDistance(str1, str2)
      distanceMatrix(i)(j) = distance
      distanceMatrix(j)(i) = distance
    }

    distanceMatrix.map(_.map(x => "%.4f".formatted(x)).mkString(" ")).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_pdst.txt"
    val data = readFile(filename)
    solve(data)
  }
}
