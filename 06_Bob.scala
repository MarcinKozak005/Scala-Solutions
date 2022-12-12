object Bob {
  def response(statement: String): String = statement match {
    case str if str.trim.endsWith("?") && str.equals(str.toUpperCase) && str.matches(".*[a-zA-Z].*") => "Calm down, I know what I'm doing!"
    case str if str.trim.endsWith("?") => "Sure."
    case str if str.equals(str.toUpperCase) && str.matches(".*[a-zA-Z].*") => "Whoa, chill out!"
    case str if str.trim.equals("") => "Fine. Be that way!"
    case _ => "Whatever."
  }
}