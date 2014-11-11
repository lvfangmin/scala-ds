package tree

import scala.math

trait Hash {
  def hash(data: Seq[T]): Int
}

object MerkleTree[+T](values: Seq[T], hashfn <: Hash) {
  private val height: Int = calRequiredHeight(values)

  def buildTree() {
    buildTreeRecursively(Range(0, values.size - 1))
  }

  def buildTreeRecursively(range: Range): Node[T] = {
    if (range.isLeaf()) {
      return Node(range, some(values[range.left]), None, None)(hashfn)
    }

    val mid = range.mid()
    val left = buildTreeRecursively(Range(range.left, mid))
    val right = buildTreeRecursively(Range(mid + 1, range.right))
    Node(range, None, left, right)(hashfn)
  }

  private def calRequiredHeight(size: Int): Int = {
    math.ceil(math.log(size) / math.log(2)).toInt
  }

  def difference(MerkleTree mt1, MerkleTree mt2): Seq[T] = {
    // TODO:
  }

  case class Node[+T, H <: Hash](range: Range, value: Option[T],
      left: Option[Node[T]], right: Option[Node[T]])(hashfn: H) {
    val hash: Int = range.isLeaf map {
      true => hashfn.hash(value.get)
      false => hashfn.hash(left.get.hash + right.get.hash)
    }
  }

  case class Range(left: Long, right: Long) {
      def isLeaf(): Boolean = {
        left == right
      }

      def mid(): Long = {
        left + (right - left) / 2
      }
  }
}
