import scala.collection.mutable

object P08_Etl {
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] = {
    val result = mutable.Map[String,Int]()
    for (pair <- scoreMap){
      for (letter <- pair._2) {
        result(letter.toLowerCase) = pair._1
      }
    }
    result.toMap
  }
}
