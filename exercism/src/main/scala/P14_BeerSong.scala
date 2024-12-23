

class P14_BeerSong {

}

object BeerSong {

  private def createVerse(verseNumber: Int): String = verseNumber match {
    case 0 => "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    case 1 => "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
    case 2 => "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
    case n => s"$n bottles of beer on the wall, $n bottles of beer.\nTake one down and pass it around, ${n - 1} bottles of beer on the wall.\n"
  }

  def recite(currentVerse: Int, numberOfVerses: Int): String =
    (currentVerse until currentVerse - numberOfVerses by -1).map(createVerse).mkString("\n")
}
