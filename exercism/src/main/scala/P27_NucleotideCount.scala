import scala.collection.mutable

class P27_NucleotideCount {

}


class DNA(dnaStrand: String) {

  def nucleotideCounts: Either[String, mutable.Map[Char, Int]] = {
    val resultMap = mutable.Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0, 'X' -> 0)
    dnaStrand.foreach {
      case nucleotide@('A' | 'C' | 'G' | 'T') => resultMap(nucleotide) = resultMap(nucleotide) + 1
      case _ => resultMap('X') = resultMap('X') + 1
    }
    if (resultMap('X') == 0) {
      resultMap.remove('X')
      Right(resultMap)
    } else Left("")
  }
}