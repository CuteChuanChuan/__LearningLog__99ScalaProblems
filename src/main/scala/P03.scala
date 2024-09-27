import scala.annotation.tailrec

object P03 {
  
  def nthBuildIn[A](n: Int, listProvided: List[A]): A = {
    if (n <= 0) throw new NoSuchElementException
    else if (n > listProvided.length) throw new NoSuchElementException
    else listProvided(n)
  }
  
  @tailrec
  def nthRecursive[A](n: Int, listProvided: List[A]): A = (n, listProvided) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case _ => throw new NoSuchElementException
  }
  
  def nthDrop[A](n: Int, listProvided: List[A]): A = n match {
    case _ if n < 0 => throw new NoSuchElementException
    case _ => listProvided.drop(n).headOption.getOrElse(throw new NoSuchElementException)
  }
  
  
}
