import scala.annotation.tailrec

class P28_WordCount {
}

class WordCount(sentence: String) {

  @tailrec
  private def occurrencesMap(list: List[String], resultMap: Map[String, Int] = Map.empty): Map[String, Int] = {
    def incrementCount(map: Map[String, Int], key: String) =
      map.updated(key, if (map.contains(key)) map(key) + 1 else 1)

    list match {
      case head :: Nil => incrementCount(resultMap, head)
      case head :: tail => occurrencesMap(tail, incrementCount(resultMap, head))
    }
  }

  def countWords: Map[String, Int] = occurrencesMap(
    sentence
      .toLowerCase
      .replaceAll("( '|' )", " ")
      .replaceAll("[^A-Za-z0-9'\\s]", " ")
      .replaceAll("\\s+", " ")
      .trim() // exercism doesn't recognize strip()
      .split(" ")
      .toList
  )
}


object WordCount {
  def apply(sentence: String): WordCount = new WordCount(sentence)
}


