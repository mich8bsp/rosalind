import Rosalind._

object ProbRevp {

  private def isReversePalindrome(str: String): Boolean = {
    str.map(DnaComplements(_)) == str.reverse
  }

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)

    val dna = fasta.map(_.dna).mkString

    val reversePalindromesPosAndLength = dna.indices.flatMap(startIdx => {
      (4 to 12).map(_ + startIdx)
        .filter(_ <= dna.length)
        .flatMap(stopIdx => Some(dna.slice(startIdx, stopIdx)).filter(isReversePalindrome))
        .map(palindrome => (startIdx+1, palindrome.length))
    })

    reversePalindromesPosAndLength.foreach {
      case (pos, length) => println(s"$pos $length")
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_revp.txt"
    val data = readFile(filename)
    solve(data)
  }
}
