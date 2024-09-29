import scala.annotation.tailrec
import scala.util.boundary, boundary.break

object P06 {
  
  def isPalindromeBuildIn[A](listProvided: List[A]): Boolean = listProvided == listProvided.reverse
  
  @tailrec
  def isPalindromeRecursive[A](listProvided: List[A]): Boolean = listProvided match {
    case Nil => true
    case singleElement :: Nil => true
    case null => throw new NullPointerException
    case _ => listProvided.head == listProvided.last && isPalindromeRecursive(listProvided.drop(1).dropRight(1))
  }
  
  def isPalindromeLoop[A](listProvided: List[A]): Boolean = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.length > 1) {
      boundary:
        var head = 0
        var tail = listProvided.length - 1
        for (_ <- head to listProvided.length / 2) {
          if (listProvided(head) != listProvided(tail)) break(false)
          head += 1
          tail -= 1
        }
        true
    }
    else true
  }
  
}
