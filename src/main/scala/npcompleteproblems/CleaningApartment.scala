package npcompleteproblems

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

/*
  This is a Hamiltonian path
*/
object CleaningApartment {

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)

    // read a graph ------------------------
    val verticesCount = s.nextInt() // rooms
    val edgesCount = s.nextInt() // corridors

    // We must be able to quickly lookup whether 2 vertices share an edge
    val adjacencies = new Array[ArrayBuffer[Int]](verticesCount + 1) // 1-based

    for (i <- 1 until verticesCount + 1) // initialize an empty list for each vertex
      adjacencies(i) = new ArrayBuffer[Int]()

    for (_ <- 0 until edgesCount) {
      val v1 = s.nextInt()
      val v2 = s.nextInt()
      adjacencies(v1).append(v2)
      adjacencies(v2).append(v1)
    }
    // done reading a graph ----------------

    val clauses = booleanSat(verticesCount, adjacencies);

    // first line: number of clauses - number of variables
    // (vertexCount possible positions for every vertex)
    println(clauses.size + " " + verticesCount * verticesCount);

    // Next lines CNF clauses, 1 per line, end with 0
    clauses.foreach(c => println(c + "0"))
  }

  private def booleanSat(vertices: Int, adjacencies: Array[ArrayBuffer[Int]]): ArrayBuffer[String] = {

    val clauses = new ArrayBuffer[String]();

    // Total number of vertices (and thus positions) implicitly passed to verPos
    implicit val posCount: Int = vertices;

    // Requirement: Each vertex must have a position
    // (1,1) || (1,2) || (1,3) ... || (1,n)
    for (vertex <- 1 to vertices) { // 1-based
      val s = new StringBuilder()
      for (pos <- 1 to posCount) s.append(verPos(vertex, pos) + " ")
      clauses.append(s.toString())
    }

    // Requirement: Each vertex can have only 1 position
    // & ( ~(1,1) || ~(1,2) )
    for (
      vertex <- 1 to vertices; // 1-based
      (pos1, pos2) <- pairs
    )
      clauses.append(-verPos(vertex, pos1) + " " + -verPos(vertex, pos2) + " ")

    // Requirement: Each position is occupied by a vertex
    // (1,1) || (2,1) || (3,1) ... || (n,1)
    for (pos <- 1 to posCount) { // 1-based
      val s = new StringBuilder()
      for (vertex <- 1 to vertices) s.append(verPos(vertex, pos) + " ")
      clauses.append(s.toString())
    }

    // Requirement: Each position is occupied by only 1 vertex
    // & ( ~(1,1) || ~(2,1) )
    for (
      position <- 1 to posCount; // 1-based
      (vert1, vert2) <- pairs
    )
      clauses.append(-verPos(vert1, position) + " " + -verPos(vert2, position) + " ")

    // Requirement: Successive vertices in the path are connected.
    // Since not all edges need be part of the path, we must restate that to:
    // Unconnected vertices cannot be successive in the path (bidirectional).
    for (
      (v1, v2) <- pairs; // pair of vertices
      pos <- 1 until posCount
      if !hasEdge(v1, v2, adjacencies)
    ) {
      clauses.append(-verPos(v1, pos) + " " + -verPos(v2, pos+1) + " ")
      clauses.append(-verPos(v2, pos) + " " + -verPos(v1, pos+1) + " ")
    }

    clauses
  }

  /*
    Rewrite a vertex-position pair to an Int value
  */
  private def verPos(vertex: Int, position: Int)(implicit posCount: Int): Int =

    (vertex - 1) * posCount + position

  /*
    All combinations of 2 positions/vertices
    Regardless of order: (2,3) equals (3,2)
  */
  private def pairs(implicit pCount: Int) =

    for (
      p1 <- 1 to pCount;
      p2 <- 1 to pCount
      if p2 > p1
    )
    yield (p1, p2)

  private def hasEdge(vert1: Int, vert2: Int, adjacencies: Array[ArrayBuffer[Int]]) =
    adjacencies(vert1).contains(vert2)

}
