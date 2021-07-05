package com.intenthq.challenge

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (​1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (​23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  class Trie() {
    var char: Option[Char] = None
    val children: Array[Trie] = Array.ofDim[Trie](10)

    def child(digit: Int): Option[Trie] = Option(children(digit))

    def insert(key: Int, char: Char): Unit =
      key.toString.foldLeft(this) { (node, ch) => node.childGetOrCreate(ch.asDigit) }.char = Some(char)

    private def childGetOrCreate(digit: Int): Trie =
      child(digit).getOrElse {
        val node = new Trie
        children(digit) = node
        node
      }

  }

  def deciphe(map: Map[Int, Char]): List[Int] => String = {
    val trie = new Trie()
    map.foreach { case (key, char) => trie.insert(key, char) }

    message => {
      val sb = new StringBuilder

      def recur(digits: List[Int], lastChar: Option[Char], lastDigits: List[Int], prevNode: Trie) {
        digits match {
          case digit :: tail =>
            prevNode.child(digit) match {
              case None =>
                val newDigits =
                  if (lastChar.nonEmpty) {
                    sb.append(lastChar.get)
                    lastDigits ::: digits
                  } else if (lastDigits.nonEmpty) {
                    sb.append(lastDigits.head)
                    lastDigits.drop(1) ::: digits
                  } else {
                    sb.append(digit)
                    tail
                  }
                recur(newDigits, None, List(), trie)
              case Some(node) =>
                val newLastDigits = if (node.char.isEmpty) lastDigits :+ digit else List()
                recur(tail, node.char.orElse(lastChar), newLastDigits, node)
            }
          case _ =>
            lastChar.map(sb.append)
            if (lastDigits.nonEmpty) {
              sb.append(lastDigits.head)
              recur(lastDigits.drop(1), None, List(), trie)
            }
        }
      }

      recur(message, None, List(), trie)
      sb.toString
    }
  }

}