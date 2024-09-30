import scala.annotation.tailrec

object P07 {
  
  def flattenBuildIn[A](listProvided: List[List[A]]): List[A] = listProvided.flatten
  
  def flattenBuiltIn2[A](listProvided: List[List[A]]): List[A] = listProvided.foldLeft(Nil: List[A])(_ ::: _)
  
  def flattenBuiltIn3[A](listProvided: List[List[A]]): List[A] = listProvided.flatMap(identity)
  
  @tailrec
  def flattenRecursive[A](listProvided: List[List[A]], res: List[A] = Nil): List[A] = listProvided match {
    case null => throw new NullPointerException
    case h :: tail => flattenRecursive(tail, res ::: h)
    case _ => res
  }
  
  def flattenLoop[A](listProvided: List[List[A]]): List[A] = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.isEmpty) Nil
    else {
      var res: List[A] = Nil
      for (i <- listProvided) res = res ::: i
      res
    }
    
  }
  
}
