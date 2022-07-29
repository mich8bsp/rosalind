import Rosalind._

import scala.collection.mutable

object ProbLexv {

  def solve(data: List[String]): Any = {
    val alphabet: List[Char] = data.head.split(" ").map(_.head).toList
    val n: Int = data(1).toInt

    var dictionary: List[String] = alphabet.map(_.toString)
    (1 until n).foreach(i => {
      dictionary = dictionary.flatMap(word => {
        if (word.length == i) {
          word :: alphabet.map(c => s"$word$c")
        } else {
          List(word)
        }
      })
    })

    dictionary.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_lexv.txt"
    val data = readFile(filename)
    solve(data)
  }
}
