class P16_FoodChain {

}


object FoodChain {

  private val Animals = Seq("fly", "spider", "bird", "cat", "dog", "goat", "cow")
  private val Comments = Seq(
    "It wriggled and jiggled and tickled inside her.",
    "How absurd to swallow a bird!",
    "Imagine that, to swallow a cat!",
    "What a hog, to swallow a dog!",
    "Just opened her throat and swallowed a goat!",
    "I don't know how she swallowed a cow!",
  )
  private val Reasons = Seq(
    "She swallowed the cow to catch the goat.",
    "She swallowed the goat to catch the dog.",
    "She swallowed the dog to catch the cat.",
    "She swallowed the cat to catch the bird.",
    "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
    "She swallowed the spider to catch the fly.",
    "I don't know why she swallowed the fly. Perhaps she'll die."
  )

  def iKnowIntroduction(animal: String) = s"I know an old lady who swallowed a $animal."

  def recite(start: Int, end: Int): String = {
    ((start - 1) to (end - 1))
      .map {
        case n if n == 0 =>
          iKnowIntroduction(Animals(n)) + "\n" + Reasons.takeRight(n+1).mkString
        case n if n == 7 =>
          iKnowIntroduction("horse") + "\nShe's dead, of course!"
        case n =>
          iKnowIntroduction(Animals(n)) +"\n" + Comments(n-1) + "\n" + Reasons.takeRight(n+1).mkString("\n")
      }.mkString("\n\n")
      .appendedAll("\n\n")
  }
}

object Main extends App {
  print(FoodChain.recite(1,3))
}