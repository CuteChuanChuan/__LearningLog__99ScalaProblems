object P09 {
  
  def packRecursive[A](listProvided: List[A]): List[List[A]] = listProvided match {
    case null => throw new NullPointerException
    case Nil => Nil
    case h :: tail => listProvided.takeWhile(_ == h) :: packRecursive(tail.dropWhile(_ == h))
  }
  
  def packRecursive2[A](listProvided: List[A]): List[List[A]] = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.isEmpty) Nil
    else {
      val (pack, rest) = listProvided span (_ == listProvided.head)
      if (rest == Nil) List(pack)
      else pack :: packRecursive2(rest)
    }
  }
  
  def packFoldLeft[A](listProvided: List[A]): List[List[A]] = {
    if (listProvided == null) throw new NullPointerException
    listProvided.foldLeft(Nil: List[List[A]]) { (acc, element) =>
      acc match {
        case Nil => List(List(element))
        case h :: tail if (h.head == element) => (element :: h) :: tail
        case _ => List(element) :: acc
      }
    }.reverse
  }
}
