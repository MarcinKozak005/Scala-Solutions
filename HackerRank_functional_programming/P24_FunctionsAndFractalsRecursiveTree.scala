import scala.io.StdIn.readInt

object Solution {

  def drawYShapedTreeBranch(
      x: Int,
      y: Int,
      length: Int,
      array: Array[Array[Char]]
  ): Array[Array[Char]] = {
    (y until y + length).foreach(array(_)(x) = '1')
    (1 to length).foreach { i =>
      array(y + length - 1 + i)(x - i) = '1' // left arm of Y
      array(y + length - 1 + i)(x + i) = '1' // right arm of Y
    }
    array
  }

  def recursiveTree(
      n: Int,
      x: Int,
      y: Int,
      length: Int,
      array: Array[Array[Char]]
  ): Array[Array[Char]] =
    n match {
      case 0 => array
      case k => {
        // left subtree
        recursiveTree(
          n - 1,
          x - length,
          y + 2 * length,
          length / 2,
          // right subtree
          recursiveTree(
            n - 1,
            x + length,
            y + 2 * length,
            length / 2,
            // main branch of left & right subtrees
            drawYShapedTreeBranch(x, y, length, array)
          )
        )
      }
    }

  def printArray(array: Array[Array[Char]]): Unit =
    array.reverse.foreach { arr =>
      println(arr.mkString(""))
    }

  def main(args: Array[String]): Unit = {
    printArray(
      recursiveTree(readInt(), 49, 0, 16, Array.fill(63, 100)('_'))
    )
  }

}
