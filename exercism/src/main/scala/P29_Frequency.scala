import scala.collection.mutable

class P29_Frequency {

}


object Frequency {
  private def increment(resultMap: mutable.Map[Char, Int], c: Char): Unit = {
    resultMap.get(c) match {
      case Some(count) => resultMap.update(c, count + 1)
      case None => resultMap.update(c, 1)
    }
  }

  def frequency(numWorkers: Int, texts: Seq[String]): mutable.Map[Char, Int] = {
    val resultMap = mutable.Map[Char, Int]()
    texts.foreach { str =>
      str.toLowerCase.foreach { c =>
        if (c.isLetter) increment(resultMap, c)
      }
    }
    resultMap
  }
}
