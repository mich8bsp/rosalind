import Rosalind._

object ProbPper {
  val MOD: Long = 1000000L

  def solve(data: List[String]): Any = {
    val Array(n, k) = data.head.split(" ").map(_.toInt)
    // choose(n, k) * k! = n! * k! / (k! * (n-k)!) = n! / (n-k) ! = (n-k+1)*(n-k+2)*...*n
    var agg: Long = 1L
    var i: Int = n - k + 1
    while (i <= n) {
      agg = (agg * i) % MOD
      i += 1
    }

    agg
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_pper.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
