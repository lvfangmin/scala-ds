package tree

/**
 * Binary Indexed Tree implementation based on:
 * http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=binaryIndexedTrees
 *
 * Problems could be solved with Binary Indexed Tree:
 * http://blog.wjin.org/posts/binary-indexed-tree.html
 */
class BinaryIndexedTree(maxIdx: Int) {
  val tree = Array[Int](maxIdx)

  /**
   * get the lowest non-zero bit
   */
  def lowbit(idx: Int): Int = idx & (-idx)

  /**
   * update idx value to value
   */
  def update(idx: Int, value: Int) {
    while (idx <= maxIdx) {
      tree[idx] += value
      idx += lowbit(idx)
    }
  }

  /**
   * sum from 1 to idx
   */
  def read(idx: Int): Int = {
    var sum:Int = 0
    while (idx > 0) {
      sum += tree[idx]
      idx -= lowbit(idx)
    }
    sum
  }

  /**
   * value at index idx
   */
  def readSingle(idx: Int): Int = {
    var sum:Int = tree[idx]
    if (idx > 0) {
      val z = idx - lowbit(idx)
      idx--
      while (idx != z) {
        sum -= tree[idx]
        idx -= lowbit(idx)
      }
    }
    sum
  }

  /**
   * sum from range [idx1, idx2]
   */
  def readRange(idx1: Int, idx2: Int): Int = {
    read(idx2) - read(idx1)
  }

  /**
   * find the index with given accumulate sum
   * could use binary search if all values are non-negative
   */
  def find(accumulateSum: Int): Int = ???
}
