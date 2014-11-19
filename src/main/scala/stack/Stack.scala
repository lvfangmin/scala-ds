package stack

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

/**
 * Another version, create stack based on List
 */
class Stack[+A](self: List[A]) {

  // The top of this stack.
  def top: A = self.head

  // The rest of this stack.
  def rest: Stack[A] = new Stack(self.tail)

  // Checks whether this stack is empty or not.
  def isEmpty: Boolean = self.isEmpty

  /**
   * Pops top element from this stack.
   */
  def pop: (A, Stack[A]) = (top, rest)

  /**
   * Pushes given element 'x' into this stack.
   */
  def push[B >: A](x: B): Stack[B] = new Stack(x :: self)
}

object Stack {

   /**
    * Returns an empty stack instance.
    */
   def empty[A]: Stack[A] = new Stack(Nil)

   /**
    * Creates a new stack from given 'xs' sequence.
    */
   def apply[A](xs: A*): Stack[A] = {
    var r: Stack[A] = Stack.empty
    for (x <- xs) r = r.push(x)
    r
  }
}

object Stack extends App {
  var stack = new Stack[Int]
  stack.push(1)
  stack.push(2)
  println(stack.pop())
  println(stack.next())
}
