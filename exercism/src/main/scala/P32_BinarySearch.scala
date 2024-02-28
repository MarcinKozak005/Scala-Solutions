import scala.annotation.tailrec

class P32_BinarySearch {

}

object BinarySearch {

  def find(list: List[Int], element: Int): Option[Int] = {
    val vector = list.toVector.sorted
    binarySearch(vector, 0, vector.length - 1, element)
  }

  @tailrec
  def binarySearch(collection: Vector[Int], start: Int, end: Int, element: Int): Option[Int] = {
    val middle = start + (end - start) / 2
    if (collection.isEmpty || start > end) None
    else collection(middle) match {
      case less if less < element => binarySearch(collection, middle + 1, end, element)
      case more if more > element => binarySearch(collection, start, middle - 1, element)
      case _ => Some(middle)
    }
  }

}