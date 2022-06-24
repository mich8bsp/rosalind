import Rosalind._

object ProbSubs {

  def solve(data: List[String]): Any = {
    val List(s, t) = data
    s.sliding(t.length)
      .zipWithIndex
      .flatMap {
        case (window, idx) if window == t => Some(idx + 1)
        case _ => None
      }
      .mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_subs.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
