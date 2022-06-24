import scala.io.Source

object Rosalind {
  val DnaNucleotides = List('A', 'C', 'G', 'T')

  val DnaComplements = Map(
    'A' -> 'T',
    'T' -> 'A',
    'C' -> 'G',
    'G' -> 'C'
  )

  val DnaToRna = Map(
    'A' -> 'A',
    'C' -> 'C',
    'G' -> 'G',
    'T' -> 'U'
  )

  val RnaToDna = Map(
    'A' -> 'A',
    'C' -> 'C',
    'G' -> 'G',
    'U' -> 'T'
  )

  val RnaComplements = DnaComplements.map {
    case (k, v) => DnaToRna(k) -> DnaToRna(v)
  }

  val RnaToAminoAcid = readFile("RnaToAminoAcidsTable.txt")
    .flatMap(_.split("  "))
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(x => {
      val Array(rna, aminoAcid) = x.split(" ")
      rna -> aminoAcid
    })
    .toMap

  case class FastaSegment(label: String, dna: String) {
    lazy val gcContent: Double = {
      val totalLength = dna.length
      totalLength match {
        case 0 => 0D
        case _ => dna.count {
          case 'C' | 'G' => true
          case _ => false
        }.toDouble * 100D / totalLength
      }
    }
  }

  def parseFasta(data: List[String]): List[FastaSegment] = data match {
    case Nil => Nil
    case _ =>
      var segmentEnd: Int = data.indexWhere(_.startsWith(">"), 1)
      if (segmentEnd < 0) {
        segmentEnd = data.length
      }
      FastaSegment(
        data.head.drop(1),
        data.slice(1, segmentEnd).mkString("")
      ) :: parseFasta(data.slice(segmentEnd, data.length))
  }

  def readFile(fileName: String): List[String] = {
    val source = Source.fromResource(fileName)

    try {
      source.getLines()
        .toList
    } finally {
      source.close()
    }
  }
}
