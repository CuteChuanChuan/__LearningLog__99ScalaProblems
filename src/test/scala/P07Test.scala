import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P07._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P07Test extends AnyFunSuiteLike {
  
  test("flatten should return the flattened list of a list of lists") {
    val list = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    P07.flattenBuildIn(list) `should` be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    P07.flattenBuiltIn2(list) `should` be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    P07.flattenBuiltIn3(list) `should` be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    P07.flattenRecursive(list) `should` be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    P07.flattenLoop(list) `should` be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
  
  test("flatten should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P07.flattenBuildIn(null)
    an[NullPointerException] should be thrownBy P07.flattenBuiltIn2(null)
    an[NullPointerException] should be thrownBy P07.flattenBuiltIn3(null)
    an[NullPointerException] should be thrownBy P07.flattenRecursive(null)
    an[NullPointerException] should be thrownBy P07.flattenLoop(null)
  }
  
  test("flatten should return an empty list if the list is empty") {
    val list = List()
    P07.flattenBuildIn(list) `should` be(List())
    P07.flattenBuiltIn2(list) `should` be(List())
    P07.flattenBuiltIn3(list) `should` be(List())
    P07.flattenRecursive(list) `should` be(List())
    P07.flattenLoop(list) `should` be(List())
  }
  
  
}
