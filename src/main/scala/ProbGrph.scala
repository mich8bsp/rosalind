import Rosalind._

object ProbGrph {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)

    val fastaBySuffixCodon: Map[String, List[FastaSegment]] = fasta
      .map(x => x.dna.slice(x.dna.length - 3, x.dna.length) -> x)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    val graph: List[(String, String)] = fasta.flatMap(x => fastaBySuffixCodon.getOrElse(x.dna.slice(0, 3), Nil)
      .filter(_.label != x.label)
      .map(y => y.label -> x.label))

    graph.foreach {
      case (s, t) => println(s"$s $t")
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_grph.txt"
    val data = readFile(filename)
    solve(data)
  }
}
