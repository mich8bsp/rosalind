import Rosalind._

import scala.collection.mutable
import scala.util.Try

object ProbOrf {

 def findCandidateProteins(rna: String): Set[String] = {
   val startCodon = "AUG"
   var idx = 0
   val candidateProteins = mutable.Set[String]()
   while (idx < rna.length) {
     val startCodonIdx = rna.indexOfSlice(startCodon, idx)
     if (startCodonIdx >= 0) {
       val proteinOpt = Try {
         rna.drop(startCodonIdx)
           .grouped(3)
           .map(RnaToAminoAcid(_))
           .takeWhile(_ != "Stop")
           .mkString
       }.filter(_.length * 3 + startCodonIdx < rna.length)

       proteinOpt.foreach(candidateProteins.add)
       idx = idx + 3
     } else {
       idx = rna.length
     }
   }

   candidateProteins.toSet
 }

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)

    val rna = fasta.map(_.dna).mkString.map(DnaToRna(_))
    val rnaReverseStrand = rna.reverse.map(RnaComplements(_))

    (findCandidateProteins(rna) ++ findCandidateProteins(rnaReverseStrand))
      .foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_orf.txt"
    val data = readFile(filename)
    solve(data)
  }
}
