import Rosalind._

object ProbLcsm {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)

    def findCommonSubstringOfLength(length: Int): Option[String] = {
      val substringOptions: List[String] = fasta.head.dna.sliding(length).toList

      substringOptions.find(substring => fasta.forall(_.dna.contains(substring))) match {
        case Some(found) => Some(found)
        case None if length > 1 => findCommonSubstringOfLength(length - 1)
        case None => None
      }
    }

    findCommonSubstringOfLength(fasta.head.dna.length).getOrElse("")

  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_lcsm.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
