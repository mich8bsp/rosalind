import Rosalind._

object ProbGc {

  def solve(data: List[String]): Any = {
    val fastaSegments = parseFasta(data)

    val fastaSegmentWithHighestGc = fastaSegments.maxBy(_.gcContent)

    println(fastaSegmentWithHighestGc.label)
    println(fastaSegmentWithHighestGc.gcContent)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_gc.txt"
    val data = readFile(filename)
    solve(data)
  }
}
