import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P04._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P04Test extends AnyFunSuiteLike {
  test("length should return the length of a list consisting integers") {
    val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    P04.lengthBuildIn(intList) `should` be(10)
    P04.lengthRecursive(intList) `should` be(10)
    P04.lengthFoldLeft(intList) `should` be(10)
  }
  
  test("length should return the length of a list consisting strings") {
    val stringList = List("a", "b", "c", "d")
    P04.lengthBuildIn(stringList) `should` be(4)
    P04.lengthRecursive(stringList) `should` be(4)
    P04.lengthFoldLeft(stringList) `should` be(4)
  }
  
  test("length should return 0 if the list is empty") {
    val emptyList = List()
    P04.lengthBuildIn(emptyList) `should` be(0)
    P04.lengthRecursive(emptyList) `should` be(0)
    P04.lengthFoldLeft(emptyList) `should` be(0)
  }
  
  test("length should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P04.lengthBuildIn(null)
    an[NullPointerException] should be thrownBy P04.lengthRecursive(null)
    an[NullPointerException] should be thrownBy P04.lengthFoldLeft(null)
  }
}
