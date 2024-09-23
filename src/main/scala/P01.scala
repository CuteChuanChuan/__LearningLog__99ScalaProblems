import scala.annotation.tailrec

object P01 {
  
  def lastBuiltIn[A](listProvided: List[A]): A = listProvided.last
  
  @tailrec
  def lastRecursive[A](listProvided: List[A]): A = listProvided match {
    case h::Nil => h
    case _::tail => lastRecursive(tail)
    case _ => throw new NoSuchElementException
  }
  
  def lastMatch[A](listProvided: List[A]): A = listProvided match {
    case _ :+ last => last
    case _ => throw new NoSuchElementException
  }
}
