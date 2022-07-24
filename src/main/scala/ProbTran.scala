import Rosalind._

object ProbTran {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)
    val s1 = fasta.head.dna
    val s2 = fasta(1).dna

    val (transitions, transversions): (Int, Int) = s1.zip(s2).map {
      case ('A', 'G') | ('G', 'A') | ('C', 'T') | ('T', 'C') => (1, 0)
      case (x, y) if x == y => (0, 0)
      case _ => (0, 1)
    } reduceLeft { (x: (Int, Int), y: (Int, Int)) => (x._1 + y._1, x._2 + y._2)
    }

    transitions.toDouble / transversions
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_tran.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
