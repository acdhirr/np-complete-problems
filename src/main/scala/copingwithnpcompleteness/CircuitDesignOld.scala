package copingwithnpcompleteness

import java.util
import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CircuitDesignOld {

  private class Vertex(
    val id: Int,
    val neighbours: ArrayBuffer[Vertex] = ArrayBuffer[Vertex](),
    var index: Int = -1,
    var lowLink: Int = 0,
    var visited: Boolean = false,
    var processed: Boolean = false
  ){
    override def toString = s"Vertex(id=$id, index=$index, lowLink=$lowLink, processed=$processed, neighbours=${neighbours.size})"
    def is(other: Vertex): Boolean = other.id.equals(this.id)
  }

  private class SCC(
    val members: mutable.Set[Int] = mutable.Set[Int]()
  ){
    def add(i: Int): Unit = members.add(i)
    override def toString = s"SCC(members=$members, size=${members.size})"
  }

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    val varCount: Int = s.nextInt()
    val clausesCount = s.nextInt()

    testTarjan()
    //test(varCount, clausesCount)

    /*
    val clauses = new Array[(Int,Int)](clausesCount) // 1-based
    for (i <- 0 until clausesCount)
      clauses(i) = (s.nextInt(), s.nextInt())

    val dirGraph = directedGraph(clauses, varCount)
    val sccList = tarjan2(dirGraph)

    println(solve(sccList,varCount))
    */

  }

  // Construct a directed graph with 2 sided implications from clauses
  private def directedGraph(clauses: Array[(Int, Int)], varCount: Int) = {

    val graph = scala.collection.mutable.Map[Int,Vertex]() // variables + negated variables

    for (i <- 1 until varCount + 1) {
      graph.put(i, new Vertex(id = i))
      graph.put(-i, new Vertex(id = -i))
    }

    for ( (x,y) <- clauses ) {
      // Implications as directed edges
      graph(-x).neighbours.append(graph(y))
      graph(-y).neighbours.append(graph(x))
    }

    graph.values
  }

  private def tarjan(graph: Iterable[Vertex]): ArrayBuffer[SCC] = {

    // A collection of SCCs, empty at the start
    val sccArray: ArrayBuffer[SCC] = ArrayBuffer[SCC]()

    var nextIndex = 0
    val stack: util.Stack[Vertex] = new util.Stack[Vertex]

    // Recursively connect everything into SCCs
    for (vertex <- graph) {
      if (!vertex.visited) connect(vertex)
    }

    def connect(vertex: Vertex): Unit = {

      // handle this vertex
      vertex.index = nextIndex
      vertex.lowLink = nextIndex
      nextIndex += 1
      vertex.visited = true
      stack.push(vertex)

      // recursively handle this vertex' neighbours
      for (neighbour <- vertex.neighbours) {
        if (!neighbour.visited) {
          connect(neighbour)
          vertex.lowLink = Math.min(vertex.lowLink, neighbour.lowLink)
        }
        else if (!neighbour.processed) {
          vertex.lowLink = Math.min(vertex.lowLink, neighbour.index)
        }
      }

      vertex.processed = true

      // this vertex is the nominator of this SCC
      if (vertex.lowLink == vertex.index) {

        val scc = new SCC()

        var stackVertex: Vertex = null;
        do {
          stackVertex = stack.pop()
          scc.add(stackVertex.id)
        }
        while (!stackVertex.is(vertex))

        sccArray.append(scc) // topologically sorted
      }
    }

    // Return collection of SCCs, together containing all vertices
    sccArray
  }


  private def tarjan2(graph: Iterable[Vertex]): ArrayBuffer[SCC] = {

    // A collection of SCCs, empty at the start
    val sccArray: ArrayBuffer[SCC] = ArrayBuffer[SCC]()

    var nextIndex = 0
    val callStack: util.Stack[Vertex] = new util.Stack[Vertex]
    val stack: util.Stack[Vertex] = new util.Stack[Vertex]

    // Recursively connect everything into SCCs
    for (startVertex <- graph; if !startVertex.visited) {

      callStack.push(startVertex)

      while ( !callStack.empty() ) {

        val vertex = callStack.pop()

        // handle this vertex
        vertex.index = nextIndex
        vertex.lowLink = nextIndex
        nextIndex += 1
        vertex.visited = true
        stack.push(vertex)

        // recursively handle this vertex' neighbours
        for (neighbour <- vertex.neighbours) {
          if (!neighbour.visited) {
            callStack.push(neighbour)
            vertex.lowLink = Math.min(vertex.lowLink, neighbour.lowLink)
          }
          else if (!neighbour.processed) {
            vertex.lowLink = Math.min(vertex.lowLink, neighbour.index)
          }
        }

        vertex.processed = true

        // this vertex is the nominator of this SCC
        if (vertex.lowLink == vertex.index) {

          val scc = new SCC()

          var stackVertex: Vertex = null;
          do {
            stackVertex = stack.pop()
            scc.add(stackVertex.id)
          }
          while (!stackVertex.is(vertex))

          sccArray.append(scc) // topologically sorted
        }
      }
    }

    // Return collection of SCCs, together containing all vertices
    sccArray
  }


  private def solve(connectedGraph: ArrayBuffer[SCC], varCount: Int): String = {

    val vars: Array[Integer] = new Array[Integer](varCount+1)
    // println(vars.mkString("Array(", ", ", ")"))

    // If any SCC contains a variable together with its negation return UNSATISFIABLE
    for (
      i <- 1 to varCount;
      scc <- connectedGraph
    )
      if (scc.members.contains(i) && scc.members.contains(-i))
        return "UNSATISFIABLE"

    // Since its not unsatisfiable, find a solution
    var solvedVars = 0
    for (
      scc <- connectedGraph;
      v <- scc.members;
      if vars(Math.abs(v)) == null;
      if solvedVars < varCount
    ) {
      vars(Math.abs(v)) = v
      solvedVars += 1
    }

    val s: StringBuilder = new StringBuilder();

    for (i <- 1 to varCount)
      s.append(s"${vars(i)} ")

    s"SATISFIABLE\n${s.toString()}"
  }

  private def testTarjan() = {

    val v1 = new Vertex(1)
    val v2 = new Vertex(2)
    val v3 = new Vertex(3)
    val v4 = new Vertex(4)
    val v5 = new Vertex(5)
    val v6 = new Vertex(6)
    val v7 = new Vertex(7)
    val v8 = new Vertex(8)
    val v9 = new Vertex(9)
    val v10 = new Vertex(10)
    /*
    v1.neighbours.append(v3)
    v3.neighbours.append(v4)
    v4.neighbours.append(v1)
    v4.neighbours.append(v5)
    v5.neighbours.append(v2)
    v2.neighbours.append(v5)
    */
    v1.neighbours.append(v2)
    v1.neighbours.append(v4)
    v2.neighbours.append(v3)
    v3.neighbours.append(v1)
    v3.neighbours.append(v4)
    v3.neighbours.append(v7)
    v4.neighbours.append(v6)
    v5.neighbours.append(v4)
    v6.neighbours.append(v5)
    v7.neighbours.append(v8)
    v8.neighbours.append(v7)
    v8.neighbours.append(v9)
    v9.neighbours.append(v10)

    val lst: Seq[Vertex] = List.apply(v1, v2, v3, v4, v5)

    lst.foreach(println(_))

    val sccList = tarjan(lst)
    sccList.foreach(println(_))
  }

  def test(varCount:Int, clauses: Int) = {

    import scala.util.Random

    val lst = new ArrayBuffer[Vertex]()
    for (i <- 1 to varCount) {
      val v = new Vertex(id=i);
      lst.append(v)
    }

    for (_ <- 1 to clauses) {

      val v1 = lst(Random.nextInt(varCount))
      val v2 = lst(Random.nextInt(varCount))
      v1.neighbours.append(v2)
    }

    val sccList = tarjan2(lst)
    println(solve(sccList, varCount))

  }

}

/*

  3 3
  1 -3
  -1 2
  -2 -3

  (x1 || ~x3) && (~x1 || ~x2) && (~x2 || ~x3)

  ~x1 => ~x3
   x3 => ~x1

   x1 => ~x2
   x2 => ~x1

   x2 => ~x3
   x3 => ~x2
                 _______
 ________       |       |
|        |      v       |
|   x1    \-> ~x1 ___   |
|      \__           |  |
|__ x2    \-> ~x2    |  |
       \__     ^     |  |
         |     |     |  |
    x3    \-> ~x3 <--|  |
     | \_______|        |
     |__________________|


*/