import scala.io.StdIn.readInt

object Solution {

  def sierpinskiTriangle(
      n: Int,
      x: Int,
      y: Int,
      width: Int,
      height: Int,
      array: Array[Array[Char]]
  ): Array[Array[Char]] =
    n match {
      case 0 => drawTriangle(x, y, height, array)
      case k =>
        // Top triangle
        sierpinskiTriangle(
          k - 1,
          x,
          y,
          width / 2,
          height / 2,
          // Right triangle
          sierpinskiTriangle(
            k - 1,
            x + (width / 4) + 1,
            y + (height / 2),
            width / 2,
            height / 2,
            // Left triangle
            sierpinskiTriangle(
              k - 1,
              x - (width / 4) - 1,
              y + (height / 2),
              width / 2,
              height / 2,
              array
            )
          )
        )
    }

    def drawTriangle(
        x: Int,
        y: Int,
        height: Int,
        array: Array[Array[Char]]
    ): Array[Array[Char]] = {
      for {
        row <- y until (y + height)
        col <- (x - (row - y)) to (x + (row - y))
      } array(row)(col) = '1'
      array
    }

  def printSierpinskiTriangle(array: Array[Array[Char]]): Unit =
    array.foreach { arr =>
      println(arr.mkString(""))
    }

  def main(args: Array[String]): Unit = {
    printSierpinskiTriangle(
      sierpinskiTriangle(readInt(), 31, 0, 63, 32, Array.fill(32, 63)('_'))
    )
  }

}
