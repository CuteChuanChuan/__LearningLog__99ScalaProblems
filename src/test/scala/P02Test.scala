import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P02._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P02Test extends AnyFunSuiteLike {
  
  test("last but one should return the last but one element of a list consisting two integers") {
    val intList = List(1, 2)
    P02.lastButOneBuildIn(intList) `should` be(1)
    P02.lastButOneMatch(intList) `should` be(1)
    P02.lastButOneRecursive(intList) `should` be(1)
    P02.lastButN(1, intList) `should` be(1)
  }
  
  test("last but one should return the last but one element of a list consisting of integers") {
    val intList = List(1, 1, 2, 3, 5, 8)
    P02.lastButOneBuildIn(intList) `should` be(5)
    P02.lastButOneMatch(intList) `should` be(5)
    P02.lastButOneRecursive(intList) `should` be(5)
    P02.lastButN(1, intList) `should` be(5)
  }
  
  test("last but one should return the last but one element of a list consisting of strings") {
    val stringList = List("a", "b", "c", "d")
    P02.lastButOneBuildIn(stringList) `should` be("c")
    P02.lastButOneMatch(stringList) `should` be("c")
    P02.lastButOneRecursive(stringList) `should` be("c")
    P02.lastButN(1, stringList) `should` be("c")
  }
  
}
