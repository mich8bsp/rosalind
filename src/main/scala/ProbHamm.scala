import Rosalind._

object ProbHamm {

  def solve(data: List[String]): Any = {
    val List(s, t) = data
    s.zip(t).map {
      case (sChar, tChar) if sChar != tChar => 1
      case _ => 0
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_hamm.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
