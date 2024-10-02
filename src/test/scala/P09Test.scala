import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P09._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P09Test extends AnyFunSuiteLike {
  
  test("pack should pack consecutive duplicates into sublists") {
    val list = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    P09.packRecursive(list) `should` be(List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
    P09.packRecursive2(list) `should` be(List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
    P09.packFoldLeft(list) `should` be(List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
  }
  
  test("pack should return an empty list if the list is empty") {
    val emptyList = List()
    P09.packRecursive(emptyList) `should` be(List())
    P09.packRecursive2(emptyList) `should` be(List())
    P09.packFoldLeft(emptyList) `should` be(List())
  }
  
  test("pack should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P09.packRecursive(null)
    an[NullPointerException] should be thrownBy P09.packRecursive2(null)
    an[NullPointerException] should be thrownBy P09.packFoldLeft(null)
  }
}
