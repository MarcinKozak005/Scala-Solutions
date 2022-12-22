object Hamming {
  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] = {
    if (dnaStrandOne.length == dnaStrandTwo.length) {
      var diff = 0
      for (i <- dnaStrandOne.indices){
        if (dnaStrandOne(i)!=dnaStrandTwo(i)) diff += 1
      }
    Option(diff)
    }
    else None
  }
}