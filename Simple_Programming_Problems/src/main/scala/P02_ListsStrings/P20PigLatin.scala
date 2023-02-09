package P02_ListsStrings

object P20PigLatin {
  // Capital letters cases (names, sentence beginnings) skipped
  def P20ListPigLatin(text: String): String = {
    text.split(" ").map(word => word.substring(1, word.length).concat(word(0).toLower.toString).concat("ay")).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    println(f"P20 L Pig Latin: ${P20ListPigLatin("The quick brown fox")}")
  }
}
