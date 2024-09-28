object P04 {
  
  def lengthBuildIn[A](listProvided: List[A]): Int = listProvided.length
  
  def lengthRecursive[A](listProvided: List[A]): Int = listProvided match {
    case Nil => 0
    case _ :: tail => 1 + lengthRecursive(tail)
    case null => throw new NullPointerException()
  }
  
  def lengthFoldLeft[A](listProvided: List[A]): Int = {
    if (listProvided == null) throw new NullPointerException
    listProvided.foldLeft(0)((count, _) => count + 1)
  }  
  
}
