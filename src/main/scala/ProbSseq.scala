import Rosalind._

import scala.collection.mutable

object ProbSseq {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)
    val s = fasta.head.dna
    val t = fasta(1).dna

    val solution = mutable.Buffer[Int]()
    var subSeqIdx = 0
    var seqIdx = 0
    while (subSeqIdx < t.length && seqIdx < s.length) {
      if (t(subSeqIdx) == s(seqIdx)) {
        solution.append(seqIdx)
        subSeqIdx += 1
      }
      seqIdx += 1
    }
    solution.map(_ + 1).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_sseq.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
