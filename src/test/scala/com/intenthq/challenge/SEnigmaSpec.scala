package com.intenthq.challenge

import org.specs2.mutable.Specification

class SEnigmaSpec extends Specification {

  section("scala")
  section("enigma")
  "SEnigmaSpec" should {
    val map = Map(23 -> 'N', 234 -> ' ', 89 -> 'H', 78 -> 'Q', 37 -> 'A')
    val deciphe = SEnigma.deciphe(map) _

    "(2,3) is 'N'" in {
      deciphe(List(2,3)) must_== "N"
    }
    "(2,3,8,9) is 'NH'" in {
      deciphe(List(2,3,8,9)) must_== "NH"
    }

    "(1,2,3) is '1N'" in {
      deciphe(List(1,2,3)) must_== "1N"
    }

    "(2,3,4) is ' '" in {
      deciphe(List(2,3,4)) must_== " "
    }

    "(1,2,3,7,3,2,3,7,2,3,4,8,9,7,8) is '1N73N7 HQ'" in {
      deciphe(List(1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)) must_== "1N73N7 HQ"
    }
  }
  section("enigma")
  section("scala")
}