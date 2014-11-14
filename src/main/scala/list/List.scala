package list

abstract sealed class List[+A] {

  // The head of this list.
  def head: A

  // The tail of this list.
  def tail: List[A]

  // Checks whether this list is empty or not.
  def isEmpty: Boolean

  /**
   * Appends the element 'x' to this list.
   */
  def append[B >: A](x: B): List[B] =
    if (isEmpty) List.make(x)
    else List.make(head, tail.append(x))

  /**
   * Prepends the element 'x' to this list.
   */
  def prepend[B >: A](x: B): List[B] = List.make(x, this)

  /**
   * Concatenates this list with given 'xs' list.
   */
  def concat[B >: A](xs: List[B]): List[B] =
    if (isEmpty) xs
    else tail.concat(xs).prepend(head)

  /**
   * Removes the element 'x' from the list.
   */
  def remove[B >: A](x: B): List[B] =
    if (isEmpty) fail("Can't find " + x + " in this list.")
    else if (x != head) List.make(head, tail.remove(x))
    else tail

  /**
   * Searches for the n-th element of this list.
   */
  def apply(n: Int): A =
    if (isEmpty) fail("Index out of the bounds.")
    else if (n == 0) head
    else tail(n - 1)

  /**
   * Checks whether this list contains element 'x' or not.
   */
  def contains[B >: A](x: B): Boolean =
    if (isEmpty) false
    else if (x != head) tail.contains(x)
    else true

  /**
   * Generates all the suffixes of this list.
   */
  def suffixes: List[List[A]] =
    if (isEmpty) List.make(List.empty)
    else tail.suffixes.prepend(this)

  /**
   * Applies the 'f' function to the each element of this list.
   */
  def foreach(f: (A) => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  /**
   * Combines all elements of this list into value.
   */
  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(l: List[A], a: B): B =
      if (l.isEmpty) a
      else loop(l.tail, op(a, l.head))

    loop(this, n)
  }

  /**
   * Creates new list by mapping this list to the 'f' function.
   */
  def map[B](f: (A) => B): List[B] =
    if (isEmpty) List.empty
    else tail.map(f).prepend(f(head))

  /**
   * Calculates the sum of all elements of this list.
   */
  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  /**
   * Calculates the product of all elements of this list.
   */
  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  /**
   * Searches for the minimal element of this list.
   */
  def min[B >: A](implicit ordering: Ordering[B]): B =
    if (isEmpty) fail("An empty list.")
    else if (tail.isEmpty) head
    else ordering.min(head, tail.min(ordering))

  /**
   * Searches for the maximal element of this list.
   */
  def max[B >: A](implicit ordering: Ordering[B]): B =
    if (isEmpty) fail("An empty list.")
    else if (tail.isEmpty) head
    else ordering.max(head, tail.max(ordering))

  /**
   * Slices this list.
   */
  def slice(from: Int, until: Int): List[A] =
    if (isEmpty || until == 0) List.empty
    else if (from == 0) tail.slice(from, until - 1).prepend(head)
    else tail.slice(from - 1, until - 1)

  /**
   * Reverses this list.
   */
  def reverse: List[A] = {
    def loop(s: List[A], d: List[A]): List[A] =
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))

    loop(this, List.empty)
  }

  /**
   * Shuffles this list.
   */
  def shuffle: List[A] = {
    val random = new scala.util.Random
    def insert(x: A, ll: List[A], n: Int): List[A] =
      ll.slice(0, n).concat(ll.slice(n, ll.length).prepend(x))

    if (isEmpty) List.empty
    else insert(head, tail.shuffle, random.nextInt(tail.length + 1))
  }

  /**
   * Generates variations of this list with given length 'k'.
   *
   * NOTES: To count number of variations the following formula can be used:
   *
   * V_k,n = n!/(n - k)!
   */
  def variations(k: Int): List[List[A]] = {
    def mixmany(x: A, ll: List[List[A]]): List[List[A]] =
      if (ll.isEmpty) List.empty
      else foldone(x, ll.head).concat(mixmany(x, ll.tail))

    def foldone(x: A, ll: List[A]): List[List[A]] =
      (1 to ll.length).foldLeft(List.make(ll.prepend(x)))((a, i) => a.prepend(mixone(i, x, ll)))

    def mixone(i: Int, x: A, ll: List[A]): List[A] =
      ll.slice(0, i).concat(ll.slice(i, ll.length).prepend(x))

    if (isEmpty || k > length) List.empty
    else if (k == 1) map(List.make(_))
    else mixmany(head, tail.variations(k - 1)).concat(tail.variations(k))
  }

  /**
   * Generates all permutations of this list.
   *
   * NOTES: To count number of permutations the following formula can be used:
   *
   * P_n = V_n,n = n!
   */
  def permutations: List[List[A]] =
    (2 to length).foldLeft(variations(1))((a, i) => variations(i).concat(a))

  /**
   * Searches for the longest increasing sub list of this list.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-3-longest-increasing-subsequence/
   */
  def longestIncreasingSubsequence[B >: A](implicit ordering: Ordering[B]): List[B] = {
    def init(i: Int, l: List[A], m: Map[Int, List[A]]): Map[Int, List[A]] =
     if (l.isEmpty) m
     else init(i + 1, l.tail, m + (i -> List(l.head)))

    def loop(i: Int, l: List[A], m: Map[Int, List[A]]): List[A] =
      if (l.isEmpty) m.maxBy(_._2.length)._2.reverse
      else {
        val f = m.filter(p => p._1 < i && ordering.lt(p._2.head, l.head))
        if (f.isEmpty) loop(i + 1, l.tail, m)
        else {
          val (_, ll) = f.maxBy(_._2.length)
          loop(i + 1, l.tail, m + (i -> ll.prepend(l.head)))
        }
      }

    if (isEmpty) List.empty
    else loop(1, tail, init(0, this, Map[Int, List[A]]()))
  }

  /**
   * Searches for the longest common sub-sequence of this and 'l' lists.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-4-longest-common-subsequence/
   *
   * TODO: The DP approach can be used here to reduce the complexity to O(mn)
   *
   * Time - O(2^n)
   * Space - O(n)
   */
  def longestCommonSubsequence[B >: A](l: List[B]): List[B] = {
    def loop(a: List[A], b: List[B], c: List[B]): List[B] =
      if (a.isEmpty || b.isEmpty) c
      else if (a.head == b.head) loop(a.tail, b.tail, c.prepend(a.head))
      else {
        val la = loop(a.tail, b, c)
        val lb = loop(a, b.tail, c)
        if (la.length > lb.length) la else lb
      }

    loop(reverse, l.reverse, List.empty)
  }

  /**
   * Returns the number of inversions that required to make this list sorted.
   *
   * http://www.geeksforgeeks.org/counting-inversions/
   *
   * TODO: The Divide-And-Conquer approach can be used here to reduce the complexity to O(n log n)
   *       due to merge sort.
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def inversions[B >: A](implicit ordering: Ordering[B]): Int = {
    def loop(x: A, t: List[A], i: Int): Int =
      if (t.isEmpty) i
      else if (ordering.gt(x, t.head)) loop(x, t.tail, i + 1)
      else loop(x, t.tail, i)

    if (isEmpty) 0
    else tail.inversions(ordering) + loop(head, tail, 0)
  }

  /**
   * Count the largest sum of contiguous sub list.
   *
   * http://www.geeksforgeeks.org/largest-sum-contiguous-subarray/
   *
   * NOTES: uses the DP-approach based on Kadane’s algorithm.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def largestSumOfContiguousSubList[B >: A](implicit num: Numeric[B]): B = {
    def loop(sm: B, gm: B, l: List[B]): B =
      if (l.isEmpty) gm
      else {
        val nsm = num.max(l.head, num.plus(sm, l.head))
        loop(nsm, num.max(gm, nsm), l.tail)
      }

    if (isEmpty) fail("An empty list.")
    else loop(head, head, tail)
  }

  /**
   * Generates all the sub-sequences of this list.
   *
   * Time - O(2^n)
   * Space - O(n)
   */
  def subsequences: List[List[A]] =
    if (isEmpty) List.empty
    else {
      val ss = tail.subsequences
      ss.map(_.prepend(head)).prepend(List.make(head)).concat(ss)
    }

  /**
   * Calculates the length of this list.
   */
  def length: Int =
    if (isEmpty) 0
    else 1 + tail.length

  /**
   * Converts this list into the string representation.
   */
  override def toString: String = {
    def loop(h: A, t: List[A], s: String): String =
      if (!t.isEmpty) loop(t.head, t.tail, s + h + ", ")
      else s + h

    if (isEmpty) "List[]"
    else "List[" + loop(head, tail, "") + "]"
  }

  /**
   * Fails with given message.
   */
  def fail(m: String) = throw new NoSuchElementException(m)

  /**
   * Generates all the prefixes of this list.
   */
  def prefixes: List[List[A]] = ???

  /**
   * Builds the increasing sub-sequence with maximum sum.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-14-maximum-sum-increasing-subsequence/
   */
  def maximumSumIncreasingSubsequence: List[A] = ???

  /**
   * Returns an intersect nodes of two lists.
   *
   * http://www.geeksforgeeks.org/write-a-function-to-get-the-intersection-point-of-two-linked-lists/
   */
  def intersect[B >: A](l: List[B]): List[B] = ???

  /**
   * Returns the longest palindromic sub-sequence of this list.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-12-longest-palindromic-subsequence/
   */
  def longestPalindromicSubsequence: List[A] = ???
}

case object Nil extends List[Nothing] {
  def head: Nothing = fail("An empty list.")
  def tail: List[Nothing] = fail("An empty list.")

  def isEmpty: Boolean = true
}

case class Cons[A](head: A, tail: List[A]) extends List[A] {
  def isEmpty: Boolean = false
}

object List {

  // An empty list.
  def empty[A]: List[A] = Nil

  // A smart constructor for list's cons.
  def make[A](x: A, t: List[A] = Nil): List[A] = Cons(x, t)

  /**
   * Creates a new list from given 'xs' sequence.
   */
  def apply[A](xs: A*): List[A] = {
    var r: List[A] = List.empty
    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}
