package copingwithnpcompleteness

import java.util
import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CircuitDesign {

  private type AdjacencyMap = mutable.Map[Int, ArrayBuffer[Int]]

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val varCount: Int = s.nextInt()
    val clausesCount = s.nextInt()

    /*
    val graph = testGraph()
    val p = postOrder(invertedGraph(graph))
    println(p)
    sccIfy(graph, p)
    */

    val clauses = new Array[(Int,Int)](clausesCount) // 1-based
    for (i <- 0 until clausesCount)
      clauses(i) = (s.nextInt(), s.nextInt())

    val dirGraph = directedGraph(clauses, varCount)

    val q = postOrder(invertedGraph(dirGraph))
    println(sccIfy(dirGraph, q, varCount))

  }

  /* DFS exploring a graph, return a list containing the post order numbering
  of all nodes.
  */
  def postOrder(graph: AdjacencyMap ) = {

    val visited = mutable.Map[Int,Boolean]()
    for (node <- graph.keys) visited.put(node,false)
    val post = mutable.Map[Int, Int]()
    for (node <- graph.keys) post.put(node, -1)
    val postVisits = new mutable.MutableList[Int]()
    var count = 1 // pre and post values

    def _dfs(): Unit = {
      for (node <- graph.keys)
        if (!visited(node)) {
          _explore(node)
        }
    }

    def _explore(node: Int): Unit = {

      val stack: util.Stack[Int] = new util.Stack[Int]()
      stack.push(node)

      while( !stack.empty() ) {

        // inspect, but do not pop from stack
        val node = stack.peek()
        // assume there are no undiscovered neighbours
        var isExplored = true

        if ( !visited(node) ) {
          _preVisit(node)
          visited(node) = true
          // explore this node's list of outgoing nodes, from the back
          for (neighbour <- graph(node).reverseIterator if !visited(neighbour)) {
            // there still are undiscovered neighbours
            isExplored = false;
            // push the neighbour on top, they must be handled first
            stack.push(neighbour)
          }
        }

        // all neighbours explored ...
        if (isExplored) {
          // ... so now handle this node
          val node = stack.pop()
          // but only when its not yet handled:
          // a node may have been pushed on the stack later and already been handled
          if (post(node) == -1) _postVisit(node)
        }
      }
    }

    def _preVisit(node: Int): Unit =
      count += 1

    def _postVisit(node: Int): Unit = {
      post(node) = count
      postVisits.+=:(node)
      count += 1
    }

    // recursive version is much simpler, but may lead to stack overflow
    def _exploreRec(node: Int): Unit = {

      visited(node) = true
      _preVisit(node)
      for (neighbour <- graph(node); if !visited(neighbour)) {
        _exploreRec(neighbour)
      }
      _postVisit(node)
    }

    _dfs()

    /*
    Map(8 -> 1, 11 -> 16, 2 -> 4, 5 -> 3, 4 -> 23, 7 -> 12, 10 -> 14, 1 -> 5, 9 -> 13, 12 -> 15, 3 -> 9, 6 -> 2)
    Map(8 -> 22, 11 -> 17, 2 -> 7, 5 -> 8, 4 -> 24, 7 -> 21, 10 -> 19, 1 -> 6, 9 -> 20, 12 -> 18, 3 -> 10, 6 -> 11)
    */
    //println(post)
    // recursive:   [4, 8, 7, 9, 10, 12, 11, 6, 3, 2, 5, 1]
    //println("[4, 8, 7, 9, 10, 12, 11, 6, 3, 2, 5, 1] recursive")
    //println(postVisits)

    postVisits.toList
  }

  /* Solve this 2-SAT graph by finding all Strongly Connected Components
  and see of any of them contains a contradiction (i and -i). If not,
  greedily set variables and return as a solution
  */
  def sccIfy(graph: AdjacencyMap, startNodes: List[Int], varCount: Int): String = {

    val sccs =  new ArrayBuffer[Set[Int]]

    val handled = mutable.Map[Int,Boolean]()
    for (node <- startNodes) handled.put(node,false)

    for (node <- startNodes; if !handled(node) ) {

      val stack: util.Stack[Int] = new util.Stack[Int]()
      stack.push(node)
      val scc: mutable.Set[Int] = new mutable.HashSet[Int]()

      while ( !stack.empty() ) {
        val node = stack.pop()
        if (scc.contains(-node)) return "UNSATISFIABLE"
        else scc.add(node)
        handled(node) = true
        for (neighbour <- graph(node); if !handled(neighbour)) {
          stack.push(neighbour)
        }
      }

      sccs.append(scc.toSet)
    }

    // Since its not unsatisfiable, find a solution
    val vars: Array[Integer] = new Array[Integer](graph.size + 1)
    var solvedVars = 0
    for (
      scc <- sccs;
      v <- scc;
      if vars(Math.abs(v)) == null;
      if solvedVars <= varCount
    ) {
      vars(Math.abs(v)) = v
      solvedVars += 1
    }

    val s: StringBuilder = new StringBuilder();

    for (i <- 1 to varCount)
      s.append(s"${vars(i)} ")

    s"SATISFIABLE\n${s.toString()}"
  }

  // Construct a directed graph with 2 sided implications from clauses
  private def directedGraph(clauses: Array[(Int, Int)], varCount: Int): AdjacencyMap = {

    val graph = mutable.Map[Int, ArrayBuffer[Int]]()

    for (node <- 1 until varCount + 1) {
      graph.put(node, new ArrayBuffer[Int]())
      graph.put(-node, new ArrayBuffer[Int]())
    }

    for ((x, y) <- clauses) {
      // Implications as directed edges
      graph(-x).append(y)
      graph(-y).append(x)
    }

    graph
  }

  /* On a given graph reverse all edges to get a reversed graph
  */
  private def invertedGraph(graph: AdjacencyMap): AdjacencyMap = {

    val igraph = mutable.Map[Int, ArrayBuffer[Int]]()

    for (node <- graph.keys) // initialize
      igraph.put(node, new ArrayBuffer[Int]())

    for ((node,adj) <- graph; a <- adj) {
      igraph(a).append(node)
      igraph(a) = igraph(a).sorted
    }

    igraph
  }

  /* This is the graph on page 91 chapter 3.4.1 Dasgupta e.a,
  Figure 3.9
  */
  private def testGraph(): AdjacencyMap = {

    val graph = mutable.Map[Int, ArrayBuffer[Int]]()
    graph.put(1, new ArrayBuffer[Int]())
    graph.put(2, new ArrayBuffer[Int]())
    graph.put(3, new ArrayBuffer[Int]())
    graph.put(4, new ArrayBuffer[Int]())
    graph.put(5, new ArrayBuffer[Int]())
    graph.put(6, new ArrayBuffer[Int]())
    graph.put(7, new ArrayBuffer[Int]())
    graph.put(8, new ArrayBuffer[Int]())
    graph.put(9, new ArrayBuffer[Int]())
    graph.put(10, new ArrayBuffer[Int]())
    graph.put(11, new ArrayBuffer[Int]())
    graph.put(12, new ArrayBuffer[Int]())

    graph(1).append(2)
    graph(2).append(3)
    graph(2).append(4)
    graph(2).append(5)
    graph(3).append(6)
    graph(5).append(2)
    graph(5).append(6)
    graph(5).append(7)
    graph(6).append(3)
    graph(6).append(8)
    graph(7).append(8)
    graph(7).append(10)
    graph(8).append(11)
    graph(9).append(7)
    graph(10).append(9)
    graph(11).append(12)
    graph(12).append(10)

    graph
  }

}
