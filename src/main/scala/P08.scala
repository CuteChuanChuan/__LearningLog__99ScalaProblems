import scala.annotation.tailrec

object P08 {
  
  def compressRecursive[A](listProvided: List[A]): List[A] = listProvided match {
    case null => throw new NullPointerException
    case Nil => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }
  
  def compressTailRecursion[A](listProvided: List[A]): List[A] = {
    @tailrec
    def compressInner[B](acc: List[B], listRemained: List[B]): List[B] = listRemained match {
      case null => throw new NullPointerException
      case h :: tail => compressInner(h :: acc, tail.dropWhile(_ == h))
      case Nil => acc.reverse
      
    }
    
    compressInner(Nil, listProvided)
  }
  
  def compressFoldRight[A](listProvided: List[A]): List[A] = {
    if (listProvided == null) throw new NullPointerException
    else listProvided.foldRight(Nil: List[A]) { (element, accumulator) =>
      if (accumulator.isEmpty || accumulator.head != element) element :: accumulator
      else accumulator
    }
  }
  
  def compressFoldLeft[A](listProvided: List[A]): List[A] = {
    if (listProvided == null) throw new NullPointerException
    else listProvided.foldLeft(Nil: List[A]) { (accumulator, element) =>
      if (accumulator.isEmpty || accumulator.last != element) accumulator :+ element
      else accumulator
    }
  }
}
