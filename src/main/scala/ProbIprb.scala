import Rosalind._

object ProbIprb {

  def solve(data: List[String]): Any = {
    val Array(homDom, het, homRec) = data.head.split(" ").map(_.toDouble)
    val total: Double = homDom + het + homRec

    val probBothParentsHomRec = (homRec / total) * ((homRec-1) / (total-1))
    val probRecBothParentsHomRec = 1D

    val probBothParentsHet = (het / total) * ((het - 1) / (total - 1))
    val probRecBothParentsHet = 0.25

    val probOneParentHetOneParentHomRec = (het / total) * (homRec / (total - 1)) * 2
    val probRecOneParentHetOneParentHomRec = 0.5

    val probRec = probBothParentsHomRec * probRecBothParentsHomRec +
      probBothParentsHet * probRecBothParentsHet +
      probOneParentHetOneParentHomRec * probRecOneParentHetOneParentHomRec
    1D - probRec
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_iprb.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
