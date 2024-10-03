object P10 {
  
  def runLengthForLoop[A](listProvided: List[A]): List[(Int, A)] = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.isEmpty) return Nil
    var count: Int = 0
    var result: List[(Int, A)] = Nil
    var listRemained: List[A] = listProvided
    for (i <- listProvided) {
      if (i == listRemained.head) count += 1
      else {
        result = (count, listRemained.head) :: result
        count = 1
        listRemained = listRemained.dropWhile(_ == listRemained.head)
      }
    }
    // handle last element
    result = (count, listRemained.head) :: result
    result.reverse
  }
  
  def runLengthRecursive[A](listProvided: List[A], result: List[(Int, A)] = Nil): List[(Int, A)] = listProvided match {
    case null => throw new NullPointerException
    case Nil => result.reverse
    case h :: tail => {
      val (pack, rest) = listProvided span (_ == h)
      runLengthRecursive(rest, (pack.length, h) :: result)
    }
  }
  
  def runLengthFoldLeft[A](listProvided: List[A]): List[(Int, A)] = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.isEmpty) return Nil
    listProvided.foldLeft(Nil: List[(Int, A)])((accumulator, element) => {
      element match
        case _ if accumulator.isEmpty => (1, element) :: accumulator
        case _ if accumulator.head._2 == element => (accumulator.head._1 + 1, element) :: accumulator.drop(1) // inefficient
        case _ => (1, element) :: accumulator
    }).reverse
  }
  
  def runLengthFoldLeft2[A](listProvided: List[A]): List[(Int, A)] = {
    if (listProvided == null) throw new NullPointerException
    if (listProvided.isEmpty) return Nil
    val (compressedList, lastRun) = listProvided.foldLeft((List.empty[(Int, A)], Option.empty[(Int, A)])) {
      case ((accumulator, None), element) => (accumulator, Some((1, element)))
      case ((accumulator, Some((count, currentElement))), element) if currentElement == element => (accumulator, Some((count + 1, element)))
      case ((accumulator, Some(lastRun)), element) => (accumulator :+ lastRun, Some((1, element)))
    }
    compressedList ++ lastRun.toList
  }
  
}
