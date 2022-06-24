import Rosalind._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex
import scala.util.{Failure, Success}

object ProbMprt {
  private implicit val executionContext = ExecutionContext.global

  def getMotifLocations(fastaSegment: FastaSegment, motifRegex: Regex, motifLength: Int): List[Int] = {
    fastaSegment.dna.sliding(motifLength)
      .zipWithIndex
      .toList
      .flatMap {
        case (slice, idx) => Some(idx+1).filter(_ => motifRegex.matches(slice))
      }
  }

  def solve(data: List[String]): Any = {
    val fasta = Future.sequence(data.map(uniprotId => {
      HttpClient.get(
        url = "www.uniprot.org",
        path = s"/uniprot/$uniprotId.fasta",
      ).map(uniprotId -> _)
    }))

    fasta.onComplete {
      case Success(value) =>
        value.foreach {
          case (uniprotId, fastaLines) =>
            val motifLocations = parseFasta(fastaLines).flatMap(getMotifLocations(_, "N[^P][S|T][^P]".r, 4))
            if (motifLocations.nonEmpty) {
              println(uniprotId)
              println(motifLocations.mkString(" "))
            }
        }
      case Failure(e) => println(e)
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_mprt.txt"
    val data = readFile(filename)
    solve(data)
    while (true) {
      Thread.sleep(1000)
    }
  }
}
