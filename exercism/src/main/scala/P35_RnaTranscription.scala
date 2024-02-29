class P35_RnaTranscription {

}

object RnaTranscription {
  def toRna(dna: String): Option[String] =
    Option(dna.map {
      case 'G' => 'C'
      case 'C' => 'G'
      case 'T' => 'A'
      case 'A' => 'U'
    })
}
