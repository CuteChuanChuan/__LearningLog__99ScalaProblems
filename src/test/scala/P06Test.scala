import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import P06._
import org.scalatest.matchers.should.Matchers.{an, be, should}

class P06Test extends AnyFunSuiteLike {
  
  test("isPalindrome should return true if the list is a palindrome consisting of integers") {
    val intList = List(1, 2, 3, 2, 1)
    P06.isPalindromeBuildIn(intList) `should` be(true)
    P06.isPalindromeRecursive(intList) `should` be(true)
    P06.isPalindromeLoop(intList) `should` be(true)
  }
  
  test("isPalindrome should return true if the list is a palindrome consisting of strings") {
    val stringList = List("a", "b", "b", "a")
    P06.isPalindromeBuildIn(stringList) `should` be(true)
    P06.isPalindromeRecursive(stringList) `should` be(true)
    P06.isPalindromeLoop(stringList) `should` be(true)
  }
  
  test("isPalindrome should return false if the list is not a palindrome consisting of integers") {
    val intList = List(1, 2, 3, 4, 5)
    P06.isPalindromeBuildIn(intList) `should` be(false)
    P06.isPalindromeRecursive(intList) `should` be(false)
    P06.isPalindromeLoop(intList) `should` be(false)
  }
  
  test("isPalindrome should return false if the list is not a palindrome consisting of strings") {
    val stringList = List("a", "b", "c")
    P06.isPalindromeBuildIn(stringList) `should` be(false)
    P06.isPalindromeRecursive(stringList) `should` be(false)
    P06.isPalindromeLoop(stringList) `should` be(false)
  }
  
  test("isPalindrome should return true if the list is empty") {
    val emptyList = List()
    P06.isPalindromeBuildIn(emptyList) `should` be(true)
    P06.isPalindromeRecursive(emptyList) `should` be(true)
    P06.isPalindromeLoop(emptyList) `should` be(true)
  }
  
  test("isPalindrome should throw NullPointerException if the list is null") {
    an[NullPointerException] should be thrownBy P06.isPalindromeBuildIn(null)
    an[NullPointerException] should be thrownBy P06.isPalindromeRecursive(null)
    an[NullPointerException] should be thrownBy P06.isPalindromeLoop(null)
  }

}
