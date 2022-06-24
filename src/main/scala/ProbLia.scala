import Rosalind._

import scala.collection.mutable

object IndividualGeno {
  private val byString: Map[String, IndividualGeno] = (for {
    a1 <- Seq("a", "A")
    a2 <- Seq("a", "A")
    b1 <- Seq("b", "B")
    b2 <- Seq("b", "B")
  } yield {
    val s = s"$a1$a2$b1$b2"
    s -> IndividualGeno(a1, a2, b1, b2)
  }).toMap

  def apply(str: String): IndividualGeno = byString(str)
}

case class IndividualGeno(a1: String, a2: String, b1: String, b2: String) {
  def isTarget = IndividualGeno("AaBb") == this

  def descendantAlleles(x1: String, x2: String, y1: String, y2: String) = {
    val options = for {
      x <- Seq(x1, x2)
      y <- Seq(y1, y2)
    } yield {
      if (x.forall(_.isLower) && y.forall(_.isUpper)) s"$y$x" else s"$x$y"
    }

    options.groupBy(identity)
      .view
      .mapValues(_.size.toDouble / 4D)
      .toMap
  }

  def descendantWith(other: IndividualGeno): Map[IndividualGeno, Double] = {
    val res = for {
      (a, aProb) <- descendantAlleles(a1, a2, other.a1, other.a2)
      (b, bProb) <- descendantAlleles(b1, b2, other.b1, other.b2)
    } yield {
      IndividualGeno(s"$a$b") -> aProb * bProb
    }

    res
  }

  override def toString: String = s"$a1$a2$b1$b2"
}

object ProbLia {

  private val cache: mutable.Map[(IndividualGeno, Int), Map[Int, Double]] = mutable.Map.empty[(IndividualGeno, Int), Map[Int, Double]]

  def solve(data: List[String]): Any = {
    val Array(k, n) = data.head.split(" ").map(_.toInt)

    def calcProb(curr: IndividualGeno, generationNum: Int): Map[Int, Double] = {
      if (generationNum == k) {
        if (curr.isTarget) Map(1 -> 1D) else Map(0 -> 1D)
      } else {
        val descendants = curr.descendantWith(IndividualGeno("AaBb"))

        (for {
          (firstDescendant, firstDescendantProb) <- descendants
          (secondDescendant, secondDescendantProb) <- descendants
        } yield {
          val firstDescendantSubCalc = cache.getOrElseUpdate((firstDescendant, generationNum + 1), calcProb(firstDescendant, generationNum + 1))
          val secondDescendantSubCalc = cache.getOrElseUpdate((secondDescendant, generationNum + 1), calcProb(secondDescendant, generationNum + 1))
          (for {
            (numFromFirstDesc, probOfNumForFirstDesc) <- firstDescendantSubCalc.toSeq
            (numFromSecondDesc, probOfNumForSecondDesc) <- secondDescendantSubCalc.toSeq
          } yield {
            (numFromFirstDesc + numFromSecondDesc) -> probOfNumForFirstDesc * probOfNumForSecondDesc
          }).groupBy(_._1)
            .view
            .mapValues(_.map(_._2).sum)
            .mapValues(_ * firstDescendantProb * secondDescendantProb)
            .toMap
        }).foldLeft(Map.empty[Int, Double])(mergeMaps)
      }
    }

    val start = IndividualGeno("AaBb")
    val res = cache.getOrElseUpdate((start, 0), calcProb(start, 0))
    res.filter(_._1 >= n).values.sum
  }

  private def mergeMaps[K](map1: Map[K, Double], map2: Map[K, Double]): Map[K, Double] = {
    (map1.keySet ++ map2.keySet).map(key => {
      key -> (map1.getOrElse(key, 0D) + map2.getOrElse(key, 0D))
    }).toMap
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_lia.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
