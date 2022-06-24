import Rosalind._

object ProbIev {

  def solve(data: List[String]): Any = {
    val Array(dom2, dom1het1, dom1rec1, het2, het1rec1, rec2) = data.head.split(" ").map(_.toInt)

    val numOffspringsForEachPair: Int = 2

    val totalOffsprings: Int = (dom2 + dom1het1 + dom1rec1 + het2 + het1rec1 + rec2) * numOffspringsForEachPair

    val probRecForDom2: Double = 0D
    val probRecForDom1Het1: Double = 0D
    val probRecForDom1Rec1: Double = 0D
    val probRecForHet2: Double = 0.25
    val probRecForHet1Rec1: Double = 0.5
    val probRecForRec2: Double = 1D

    val totalRecessiveOffsprings: Double = (dom2 * probRecForDom2 +
        dom1het1 * probRecForDom1Het1 +
        dom1rec1 * probRecForDom1Rec1 +
        het2 * probRecForHet2 +
        het1rec1 * probRecForHet1Rec1 +
        rec2 * probRecForRec2) * numOffspringsForEachPair


    totalOffsprings - totalRecessiveOffsprings
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_iev.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
