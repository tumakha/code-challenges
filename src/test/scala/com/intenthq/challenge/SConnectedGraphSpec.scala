package com.intenthq.challenge

import org.specs2.mutable.Specification

class SConnectedGraphSpec extends Specification {

  section("scala")
  section("graph")
  "SConnectedGraph" should {
    "work with an acyclic graph" in {
      val dest = Node(9)
      val n10 = Node(10)
      val n8 = Node(8, List(dest))
      val start = Node(3, List(n8, n10))
      val start2 = Node(11, List(Node(2), dest, n10))
      val start3 = Node(5, List(start2))
      SConnectedGraph.run(start, dest) must_== true
      SConnectedGraph.run(start2, dest) must_== true
      SConnectedGraph.run(start, dest) must_== true
      SConnectedGraph.run(start2, dest) must_== true
      SConnectedGraph.run(start3, dest) must_== true
      SConnectedGraph.run(dest, start3) must_== false
      SConnectedGraph.run(dest, start2) must_== false
      SConnectedGraph.run(start, start2) must_== false
    }
    "a node is connected to itself" in {
      val start = Node(1)
      SConnectedGraph.run(start, start) must_== true
    }
    "handle cycle in graph" in {
      val dest = Node(20)
      val n3 = Node(3)
      val n2 = Node(2, List(n3, dest))
      val n1 = Node(1, List(n2))
      n3.edges = List(n1, n2)
      SConnectedGraph.run(n1, n1) must_== true
      SConnectedGraph.run(n1, dest) must_== true
      SConnectedGraph.run(n3, dest) must_== true
      SConnectedGraph.run(n1, n3) must_== true
      SConnectedGraph.run(dest, n1) must_== false
    }
  }
  section("graph")
  section("scala")
}