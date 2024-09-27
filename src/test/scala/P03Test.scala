import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P03._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P03Test extends AnyFunSuiteLike {
  
  test("nth should return the nth element of a list consisting several integers") {
    val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    P03.nthBuildIn(1, intList) `should` be(2)
    P03.nthBuildIn(5, intList) `should` be(6)
    P03.nthRecursive(1, intList) `should` be(2)
    P03.nthRecursive(5, intList) `should` be(6)
    P03.nthDrop(1, intList) `should` be(2)
    P03.nthDrop(5, intList) `should` be(6)
  }
  
  test("nth should return the nth element of a list consisting several strings") {
    val stringList = List("a", "b", "c", "d")
    P03.nthBuildIn(1, stringList) `should` be("b")
    P03.nthBuildIn(2, stringList) `should` be("c")
    P03.nthRecursive(1, stringList) `should` be("b")
    P03.nthRecursive(2, stringList) `should` be("c")
    P03.nthDrop(1, stringList) `should` be("b")
    P03.nthDrop(2, stringList) `should` be("c")
  }
  
  test("nth should throw NoSuchElementException if the list is empty") {
    val emptyList = List()
    an[NoSuchElementException] should be thrownBy P03.nthBuildIn(1, emptyList)
    an[NoSuchElementException] should be thrownBy P03.nthRecursive(1, emptyList)
    an[NoSuchElementException] should be thrownBy P03.nthDrop(1, emptyList)
  }
  
  test("nth exceeds the length of the list should throw NoSuchElementException") {
    val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    an[NoSuchElementException] should be thrownBy P03.nthBuildIn(11, intList)
    an[NoSuchElementException] should be thrownBy P03.nthRecursive(11, intList)
    an[NoSuchElementException] should be thrownBy P03.nthDrop(11, intList)
  }
  
  test("nth could not be negative otherwise should throw NoSuchElementException") {
    val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    an[NoSuchElementException] should be thrownBy P03.nthBuildIn(-5, intList)
    an[NoSuchElementException] should be thrownBy P03.nthRecursive(-5, intList)
    an[NoSuchElementException] should be thrownBy P03.nthDrop(-5, intList)
  }
  
}
