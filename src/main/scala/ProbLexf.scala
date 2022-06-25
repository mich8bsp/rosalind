import Rosalind._

object ProbLexf {

  def solve(data: List[String]): Any = {
    val alphabet = data.head.split(" ")
    val n = data(1).toInt

    val dictionary = (1 to n).foldLeft(List("")) {
      case (dictionary, _) =>
        dictionary.flatMap(word => alphabet.map(letter => s"$word$letter"))
    }

    dictionary.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_lexf.txt"
    val data = readFile(filename)
    solve(data)
  }
}
