package Stack

class Stack[A] extends Iterator[A] {
  var N: Int = 0
  var first: Node[A] = _

  override def isEmpty(): Boolean = {
    N == 0
  }

  override def size(): Int = {
    N
  }

  def push(item: A) {
    var newItem = new Node[A](item)
    newItem.next = first
    first = newItem
    N = N + 1
  }

  def top(): A = {
    first.value
  }

  def pop(): A = {
    if (!hasNext()) {
        throw new NoSuchElementException
    }

    var t = top()
    first = first.next
    N = N - 1
    t
  }

  override def hasNext(): Boolean = {
    N != 0
  }

  override def next(): A = {
    pop()
  }
}

class Node[A](v: A) {
  var value: A = v
  var next: Node[A] = _
}

object Stack extends App {
  var stack = new Stack[Int]
  stack.push(1)
  stack.push(2)
  println(stack.pop())
  println(stack.next())
}
