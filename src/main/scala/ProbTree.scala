import Rosalind._

import scala.collection.mutable

object ProbTree {

  def solve(data: List[String]): Any = {
    val n = data.head.toInt
    val edges = data.tail.map(_.split(" ") match {
      case Array(a, b) => (a.toInt, b.toInt)
    })

    val graph = mutable.Map[Int, mutable.Buffer[Int]]()
    (1 to n).foreach(x => graph.put(x, mutable.Buffer[Int]()))

    edges.foreach {
      case (a, b) =>
        graph(a).append(b)
        graph(b).append(a)
    }

    def getConnectedSubGraph(root: Int): mutable.Set[Int] = {
      val dfsQueue: mutable.Queue[Int] = mutable.Queue[Int]()
      val visited: mutable.Set[Int] = mutable.Set[Int]()
      dfsQueue.enqueue(root)
      while (dfsQueue.nonEmpty) {
        val current = dfsQueue.dequeue()
        visited.add(current)
        graph.getOrElse(current, mutable.Buffer[Int]())
          .filterNot(visited.contains)
          .foreach(dfsQueue.enqueue)
      }
      visited
    }

    var numOfConnectedGraphs = 0
    while (graph.nonEmpty){
      val connectedSubGraph = getConnectedSubGraph(graph.head._1)
      connectedSubGraph.foreach(graph.remove)
      numOfConnectedGraphs += 1
    }

    numOfConnectedGraphs - 1
  }

  def main(args: Array[String]): Unit = {
    val filename = "rosalind_tree.txt"
    val data = readFile(filename)
    println(solve(data))
  }
}
