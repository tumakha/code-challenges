package com.intenthq.challenge

case class Node(value: Int, var edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false
  def run(source: Node, target: Node): Boolean = {
    var visited: Set[Node] = Set()

    def recur(source: Node): Boolean = {
      source == target ||
        (!visited(source) && {
          visited += source
          source.edges.exists(recur)
        })
    }

    recur(source)
  }

}
