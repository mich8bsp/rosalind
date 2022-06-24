import Rosalind._

object ProbCons {

  def solve(data: List[String]): Any = {
    val fastaSegments = parseFasta(data)
    val fastaSegmentLength = fastaSegments.head.dna.length

    val profile = DnaNucleotides.map(nuc => nuc ->
      (0 until fastaSegmentLength).map(idx => fastaSegments.count(segment => segment.dna(idx) == nuc))
    ).toMap

    val consensus = (0 until fastaSegmentLength).map(idx => profile.maxBy(_._2(idx))._1).mkString

    println(consensus)
    DnaNucleotides.foreach(nuc => {
      println(s"$nuc: ${profile(nuc).mkString(" ")}")
    })
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_cons.txt"
    val data = readFile(filename)
    solve(data)
  }
}
