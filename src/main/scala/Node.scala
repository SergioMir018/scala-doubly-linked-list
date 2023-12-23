class Node[T](value: T, var prev: Option[Node[T]] = None, var next: Option[Node[T]] = None):
  def getValue: T = value

  def getPrev: Option[Node[T]] = prev

  def getNext: Option[Node[T]] = next

  def setPrev(item: Option[Node[T]]): Unit = {
    prev = item
  }

  def setNext(item: Option[Node[T]]): Unit = {
    next = item
  }
end Node
