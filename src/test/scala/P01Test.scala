import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P01._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P01Test extends AnyFunSuiteLike {
  
  test("last should return the last element as int of a list consisting of integers") {
    val intList = List(1, 1, 2, 3, 5, 8)
    P01.lastBuiltIn(intList) `should` be(8)
    P01.lastRecursive(intList) `should` be(8)
    P01.lastMatch(intList) `should` be(8)
  }
  
  test("last should return the last element as string of a list consisting of strings") {
    val stringList = List("a", "b", "c", "d")
    P01.lastBuiltIn(stringList) `should` be("d")
    P01.lastRecursive(stringList) `should` be("d")
    P01.lastMatch(stringList) `should` be("d")
  }
  
  test("last should throw NoSuchElementException if the list is empty") {
    val emptyList = List()
    an[NoSuchElementException] `should` be `thrownBy` P01.lastBuiltIn(emptyList)
    an[NoSuchElementException] `should` be `thrownBy` P01.lastRecursive(emptyList)
    an[NoSuchElementException] `should` be `thrownBy` P01.lastMatch(emptyList)
  }
}
