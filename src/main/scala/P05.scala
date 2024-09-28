import scala.annotation.tailrec

object P05 {
  
  def reverseBuildIn[A](listProvided: List[A]): List[A] = listProvided.reverse
  
  def reverseRecursive[A](listProvided: List[A]): List[A] = listProvided match {
    case Nil => Nil
    case h :: tail => reverseRecursive(tail) ::: List(h)
    case null => throw new NullPointerException
  }
  
  @tailrec
  def reverseMatch[A](listProvided: List[A], acc: List[A] = Nil): List[A] = listProvided match {
    case null => throw new NullPointerException
    case Nil => acc
    case h :: tail => reverseMatch(tail, h :: acc)
  }
  
  def reverseFoldLeft[A](listProvided: List[A]): List[A] = {
    if (listProvided == null) throw new NullPointerException
    listProvided.foldLeft(Nil: List[A])((h, tail) => tail :: h)
  }
  
}
