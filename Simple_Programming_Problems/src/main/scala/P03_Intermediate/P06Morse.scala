package P03_Intermediate

object P06Morse {

  val morseWordsSeparator = '/'
  val morseCharactersSeparator = ' '
  type MorseString = String
  val lettersToMorse: Map[Char, MorseString] = Map(
    'a' -> ".-",
    'b' -> "-...",
    'c' -> "-.-.",
    'd' -> "-..",
    'e' -> ".",
    'f' -> "..-.",
    'g' -> "--.",
    'h' -> "....",
    'i' -> "..",
    'j' -> ".---",
    'k' -> "-.-",
    'l' -> ".-..",
    'm' -> "--",
    'n' -> "-.",
    'o' -> "---",
    'p' -> ".--.",
    'q' -> "--.-",
    'r' -> ".-.",
    's' -> "...",
    't' -> "-",
    'u' -> "..-",
    'v' -> "...-",
    'w' -> ".--",
    'x' -> "-..-",
    'y' -> "-.--",
    'z' -> "--..",
  )

  val morseToLetters: Map[MorseString, Char] = Map.from(lettersToMorse.values.zip(lettersToMorse.keys))

  def toMorse(text: String): Option[String] = {
    text.toLowerCase.map {
      case ' ' => Some(morseWordsSeparator)
      case l => lettersToMorse.get(l)
    }.foldLeft(Option("")) {
      case (Some(acc), Some(curr)) => Some(acc.concat(s"$morseCharactersSeparator$curr"))
      case _ => None
    }
  }

  def fromMorse(text: MorseString, characterSeparator: Char = morseCharactersSeparator, wordsSeparator: Char = morseWordsSeparator): Option[String] = {
    text.split(characterSeparator).map {
      case l if l.strip() == wordsSeparator.toString => Some(' ')
      case l => morseToLetters.get(l)
    }.foldLeft(Option("")) {
      case (Some(acc), Some(curr)) => Some(acc.concat(curr.toString))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    println(toMorse("i like ducks"))
    println(fromMorse(".. / .-.. .. -.- . / -.. ..- -.-. -.- ..."))
  }

}



