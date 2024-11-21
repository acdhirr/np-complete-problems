package npcompleteproblems

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

/*
  Construct a Boolean Satisfiability Formula for the 3 colored network problem.
*/
object GSMNetwork {

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    // read a graph ------------------------
    val verticesCount = s.nextInt()
    val edgesCount = s.nextInt()
    val edges = new Array[(Int,Int)](edgesCount) // (2,4) or (4,2) are equiv.

    for (i <- 0 until edgesCount) edges(i) = (s.nextInt(), s.nextInt())
    // done reading a graph ----------------

    val clauses = booleanSat(verticesCount, edges);
    // first line: number of clauses - number of variables (one of 3 colors for every vertex)
    println( clauses.size + " " + verticesCount * 3 );

    // Next lines CNF clauses, 1 per line, end with 0
    clauses.foreach(c => println(c + "0"))
  }

  private def booleanSat(vertices: Int, edges: Array[(Int, Int)]): ArrayBuffer[String] = {

    val clauses = new ArrayBuffer[String]();

    // Requirement: Vertices must have a color
    for (vertex <- 1 to vertices) { // 1-based
      val s = new StringBuilder()
      for (color <- 1 to 3) s.append(verCol(vertex, color) + " ")
      clauses.append(s.toString())
    }

    // Requirement: Vertices can have only 1 color
    for (
      vertex <- 1 to vertices; // 1-based
      colPair <- colorPairs
    )
    clauses.append(-verCol(vertex, colPair._1) + " " + -verCol(vertex, colPair._2) + " ")

    // Requirement: Vertices connected by an edge must have different colors
    for (
      edge <- edges;
      color <- 1 to 3
    )
    clauses.append(-verCol(edge._1, color) + " " + -verCol(edge._2, color) + " ")

    clauses
  }

  /* Rewrite a vertex-color pair to an Int value
     Vertex 1 color 3 = 3
     Vertex 2 color 1 = 4
     Vertex 3 color 2 = 8
  */
  private def verCol(vertex: Int, color: Int): Int =

    (vertex-1) * 3 + color

  private def colorPairs =

    for (
      col1 <- 1 to 3;
      col2 <- 1 to 3
      if col2 > col1
    )
    yield (col1, col2)

}

/*

4 4
1 2
1 3
2 3
2 4

*/