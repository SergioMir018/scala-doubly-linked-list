class LinkedList[T](var length: Int = 0):
  private var head: Option[Node[T]] = None
  private var tail: Option[Node[T]] = None

  def prepend(item: T): Unit =
    val node = Node[T](item)
    this.length += 1

    head match
      case Some(head) =>
        node.setNext(this.head)
        head.setPrev(Some(node))
        this.head = Some(node)
      case None =>
        this.head = Some(node)
        this.tail = Some(node)

  def insertAt(item: T, index: Int): Unit =
    val node = Node[T](item)

    index match
      case i if i > this.length =>
        throw new IndexOutOfBoundsException(s"Cannot insert at index $i")
      case i if i == this.length =>
        this.append(item)
      case i if this.length == 0 =>
        this.prepend(item)

    val curr = this.head
    this.length += 1

    node.setNext(curr)
    node.setPrev(curr.get.getPrev)
    curr.get.setPrev(Some(node))

    if node.getPrev.isDefined then
      node.getPrev.get.setNext(curr)

  def append(item: T): Unit =
    this.length += 1
    val node = Node[T](item)

    this.tail match
      case Some(tail) =>
        node.setPrev(this.tail)
        tail.setNext(Some(node))
        this.tail = Some(node)
      case None =>
        this.head = Some(node)
        this.tail = Some(node)

  def remove(item: T): Option[T] =
    val (found, curr) = contains(item)

    if found then
      this.removeNode(curr)
    else
      None

  def get(index: Int): Option[T] =
    if this.getAt(index).isDefined then
      Some(this.getAt(index).get)
    else
      None

  def getAt(index: Int): Option[T] =
    var curr = this.head

    index match
      case i if i == 0 =>
        Some(curr.get.getValue)
      case i if i == this.length =>
        Some(this.tail.get.getValue)
      case i if i > this.length =>
        throw new IndexOutOfBoundsException(s"Cannot get element at index $i")
      case i if i < this.length =>
        var i = 0

        while (i < index) {
          curr = curr.get.getNext
          i += 1
        }
        Some(curr.get.getValue)

  private def removeNode(node: Node[T]): Option[T] =
    this.length -= 1

    if this.length == 0 then
      val out: Option[Node[T]] = if this.head.isDefined then this.head else None
      this.head = None
      this.tail = None
      return Some(out.get.getValue)

    if node.getPrev.isDefined then
      node.getPrev.get.setNext(node.getNext)

    if node.getNext.isDefined then
      node.getNext.get.setPrev(node.getPrev)

    if this.head.isDefined && node.getValue.equals(this.head.get.getValue) then
      this.head = node.getNext

    if this.tail.isDefined && node.getValue.equals(this.tail.get.getValue) then
      this.tail = node.getPrev

    node.setPrev(None)
    node.setNext(None)

    Some(node.getValue)

  def contains(item: T): (Boolean, Node[T]) =
    var curr = this.head
    var found = false

    while (curr.isDefined && !found)
      if curr.get.getValue.equals(item) then
        found = true
      else
        curr = curr.get.getNext

    (found, curr.get)

end LinkedList
