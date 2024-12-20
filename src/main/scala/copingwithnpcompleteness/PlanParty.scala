package copingwithnpcompleteness

import java.util
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

/*
  Independent Set, special case: the set is a tree.

                 3
               / | \
              /  |  \
             5.  1   6.
             |   |   | \
             2   3.  7  2
                   / | \
                  1. 2. 1.

  Total weight: 18 (dots are selected vertices)
*/
object PlanParty {

  private class Person(
    val funFactor: Int,
    val colleagues: ArrayBuffer[Person] = ArrayBuffer[Person](),
    var funTotal: Int = -1,
    var visited: Boolean = false
  ) {
    def isLeaf: Boolean = colleagues.isEmpty
    override def toString = s"Person(funFactor=$funFactor, funTotal=$funTotal, colleagues=${colleagues.size})"
  }

  def main(args: Array[String]): Unit = {

    val s = new Scanner(System.in)
    val personCount: Int = s.nextInt()
    val persons = new Array[Person](personCount)
    for (i <- 0 until personCount)
      persons(i) = new Person(s.nextInt)

    for (i <- 0 until personCount -1) {
      val n1 = s.nextInt() - 1  // zero based
      val n2 = s.nextInt() - 1
      persons(n1).colleagues.append(persons(n2))
      persons(n2).colleagues.append(persons(n1))
    }

    //persons.foreach(println)
    //println("============")
    println(maxFun(persons))

  }

  // Non recursive DFS traversal with a local stack
  def maxFun(persons: Array[Person]): Int = {

    val stack: util.Stack[Person] = new util.Stack[Person]()
    stack.push(persons(0))

    while ( !stack.empty() ) {

      val person = stack.peek()
      var isPoppable = true // assume there are no children

      if (!person.visited) {
        person.visited = true
        for (colleague <- person.colleagues; if !colleague.visited) {
          isPoppable = false // do not pop person, there are children first
          stack.push(colleague)
        }
      }

      // all colleagues further in the tree explored ...
      // Now get the maxFun for this person
      if (isPoppable) {
        val person = stack.pop()
        person.funTotal = maxFun(person)
      }
    }

    // return funTotal for root
    persons(0).funTotal
  }

  /*
    Inspecting a person, direct children (lower colleagues) and their
    direct children, find out what is the maximum funfactor to achieve
    for this person.
  */
  def maxFun(person: Person): Int = {

    if (person.isLeaf)
      person.funTotal = person.funFactor

    else {

      // Person + grandchildren 1-0-1
      var max1 = person.funFactor
      for (c1 <- person.colleagues; if c1.funTotal != -1) // don't count higher colleagues
        for (c2 <- c1.colleagues; if c2.funTotal != -1)
          max1 += c2.funTotal

      // Not the person nor grandchildren, only the children 0-1-0
      var max2 = 0
      for (c1 <- person.colleagues; if c1.funTotal != -1)
          max2 += c1.funTotal

      // take the max for this sub-problem
      person.funTotal = Math.max(max1, max2)
    }

    person.funTotal
  }
}


/*

1
1000
> 1000

2
1 2
1 2
> 2

5
1 5 3 7 5
5 4
2 3
4 2
1 2
> 11

11
3 5 1 6 2 3 7 2 1 2 1
1 2
1 3
1 4
5 2
6 3
7 4
8 4
7 9
7 10
7 11
> 18

*/