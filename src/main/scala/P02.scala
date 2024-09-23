import scala.annotation.tailrec

object P02 {
  def lastButOneBuildIn[A](listProvided: List[A]): A = {
    if (listProvided.isEmpty) throw new NoSuchElementException
    listProvided.reverse.tail.head
  }
  
  def lastButOneMatch[A](listProvided: List[A]): A = listProvided match {
    case _ :+ target :+ end => target
    case _ => throw new NoSuchElementException
  }
  
  @tailrec
  def lastButOneRecursive[A](listProvided: List[A]): A = listProvided match {
    case h :: _ :: Nil => h
    case _ :: tail => lastButOneRecursive(tail)
    case _ => throw new NoSuchElementException
  }
  
  def lastButN[A](n: Int, listProvided: List[A]): A =  {
    if (n <= 0) throw new NoSuchElementException
    if (listProvided.length < n) throw new NoSuchElementException
    listProvided.takeRight(n + 1).head
  }
  
}
