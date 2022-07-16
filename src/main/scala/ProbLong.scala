import Rosalind._

import scala.collection.mutable

object ProbLong {

  def solve(data: List[String]): Any = {
    val fasta = parseFasta(data)
    val prefixes = fasta.flatMap(x => (x.dna.length / 2 + 1 to x.dna.length).map(i => x.dna.substring(0, i) -> x))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    val maxPrefixSize = prefixes.keySet.map(_.length).max
    val minPrefixSize = prefixes.keySet.map(_.length).min

    val suffixes = fasta.flatMap(x => (x.dna.length / 2 + 1 to x.dna.length).map(i => x.dna.substring(x.dna.length - i, x.dna.length) -> x))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    val maxSuffixSize = suffixes.keySet.map(_.length).max
    val minSuffixSize = suffixes.keySet.map(_.length).min

    val reconstructedIds = mutable.Set[String](fasta.head.label)

    var reconstructedDna = fasta.head.dna
    var tryMatchLeft: Boolean = true
    var tryMatchRight: Boolean = true

    while (reconstructedIds.size < fasta.size) {
      var foundLeft: Boolean = false
      var suffixSize = maxSuffixSize
      if (tryMatchLeft) {
        while (!foundLeft && suffixSize >= minSuffixSize) {
          val suffix = reconstructedDna.slice(0, suffixSize)
          suffixes.get(suffix).flatMap(_.find(x => !reconstructedIds.contains(x.label))) match {
            case Some(foundForSuffix) =>
              reconstructedDna = foundForSuffix.dna + reconstructedDna.slice(suffixSize, reconstructedDna.length)
              foundLeft = true
              reconstructedIds.add(foundForSuffix.label)
            case None =>
              suffixSize -= 1
          }
        }
        if(!foundLeft) {
          tryMatchLeft = false
        }
      }

      var foundRight: Boolean = false
      var prefixSize = maxPrefixSize
      if (tryMatchRight) {
        while (!foundRight && prefixSize >= minPrefixSize) {
          val prefix = reconstructedDna.slice(reconstructedDna.length - prefixSize, reconstructedDna.length)
          prefixes.get(prefix).flatMap(_.find(x => !reconstructedIds.contains(x.label))) match {
            case Some(foundForPrefix) =>
              reconstructedDna = reconstructedDna.slice(0, reconstructedDna.length - prefixSize) + foundForPrefix.dna
              foundRight = true
              reconstructedIds.add(foundForPrefix.label)
            case None =>
              prefixSize -= 1
          }
        }
        if (!foundRight) {
          tryMatchRight = false
        }
      }
    }

    reconstructedDna
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_long.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
