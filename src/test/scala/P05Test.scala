import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P05._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P05Test extends AnyFunSuiteLike {
  
  test("reverse should return the reversed list of a list consisting integers") {
    val intList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    P05.reverseBuildIn(intList) `should` be(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    P05.reverseRecursive(intList) `should` be(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    P05.reverseMatch(intList) `should` be(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    P05.reverseFoldLeft(intList) `should` be(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
  }
  
  test("reverse should return the reversed list of a list consisting strings") {
    val stringList = List("a", "b", "c", "d")
    P05.reverseBuildIn(stringList) `should` be(List("d", "c", "b", "a"))
    P05.reverseRecursive(stringList) `should` be(List("d", "c", "b", "a"))
    P05.reverseMatch(stringList) `should` be(List("d", "c", "b", "a"))
    P05.reverseFoldLeft(stringList) `should` be(List("d", "c", "b", "a"))
  }
  
  test("reverse should return an empty list if the list is empty") {
    val emptyList = List()
    P05.reverseBuildIn(emptyList) `should` be(List())
    P05.reverseRecursive(emptyList) `should` be(List())
    P05.reverseMatch(emptyList) `should` be(List())
    P05.reverseFoldLeft(emptyList) `should` be(List())
  }
  
  test("reverse should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P05.reverseBuildIn(null)
    an[NullPointerException] should be thrownBy P05.reverseRecursive(null)
    an[NullPointerException] should be thrownBy P05.reverseMatch(null)
    an[NullPointerException] should be thrownBy P05.reverseFoldLeft(null)
  }
}
