import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P08._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P08Test extends AnyFunSuiteLike {
  
  test("compress should compress a list of integers") {
    val intList = List(1, 1, 1, 1, 2, 3, 4, 4, 4, 5, 5, 5, 5, 5)
    P08.compressRecursive(intList) `should` be(List(1, 2, 3, 4, 5))
    P08.compressTailRecursion(intList) `should` be(List(1, 2, 3, 4, 5))
    P08.compressFoldRight(intList) `should` be(List(1, 2, 3, 4, 5))
    P08.compressFoldLeft(intList) `should` be(List(1, 2, 3, 4, 5))
  }
  
  test("compress should compress a list of strings") {
    val stringList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    P08.compressRecursive(stringList) `should` be(List("a", "b", "c", "a", "d", "e"))
    P08.compressTailRecursion(stringList) `should` be(List("a", "b", "c", "a", "d", "e"))
    P08.compressFoldRight(stringList) `should` be(List("a", "b", "c", "a", "d", "e"))
    P08.compressFoldLeft(stringList) `should` be(List("a", "b", "c", "a", "d", "e"))
  }
  
  test("compress should return an empty list if the list is empty") {
    val emptyList = List()
    P08.compressRecursive(emptyList) `should` be(List())
    P08.compressTailRecursion(emptyList) `should` be(List())
    P08.compressFoldRight(emptyList) `should` be(List())
    P08.compressFoldLeft(emptyList) `should` be(List())
  }
  
  test("compress should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P08.compressRecursive(null)
    an[NullPointerException] should be thrownBy P08.compressTailRecursion(null)
    an[NullPointerException] should be thrownBy P08.compressFoldRight(null)
    an[NullPointerException] should be thrownBy P08.compressFoldLeft(null)
  }
}
