import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P10._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P10Test extends AnyFunSuiteLike {
  
  test("run-length encoding should return the run-length encoding of a list consisting of integers") {
    val intList = List(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 2, 3, 3)
    P10.runLengthForLoop(intList) `should` be(List((3, 1), (3, 2), (4, 3), (2, 1), (1, 2), (2, 3)))
    P10.runLengthRecursive(intList) `should` be(List((3, 1), (3, 2), (4, 3), (2, 1), (1, 2), (2, 3)))
    P10.runLengthFoldLeft(intList) `should` be(List((3, 1), (3, 2), (4, 3), (2, 1), (1, 2), (2, 3)))
  }
  
  test("run-length encoding should return the run-length encoding of a list consisting of integers with single last element") {
    val intList = List(1, 1, 2, 2, 2, 3, 3, 4)
    P10.runLengthForLoop(intList) `should` be(List((2, 1), (3, 2), (2, 3), (1, 4)))
    P10.runLengthRecursive(intList) `should` be(List((2, 1), (3, 2), (2, 3), (1, 4)))
    P10.runLengthFoldLeft(intList) `should` be(List((2, 1), (3, 2), (2, 3), (1, 4)))
  }
  
  test("run-length encoding should return the run-length encoding of a list consisting of strings") {
    val stringList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    P10.runLengthForLoop(stringList) `should` be(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))
    P10.runLengthRecursive(stringList) `should` be(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))
    P10.runLengthFoldLeft(stringList) `should` be(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))
  }
  
  test("run-length encoding should return an empty list if the list is empty") {
    val emptyList = List()
    P10.runLengthForLoop(emptyList) `should` be(List())
    P10.runLengthRecursive(emptyList) `should` be(List())
    P10.runLengthFoldLeft(emptyList) `should` be(List())
  }
  
  test("run-length encoding should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P10.runLengthForLoop(null)
    an[NullPointerException] should be thrownBy P10.runLengthRecursive(null)
    an[NullPointerException] should be thrownBy P10.runLengthFoldLeft(null)
  }
}
