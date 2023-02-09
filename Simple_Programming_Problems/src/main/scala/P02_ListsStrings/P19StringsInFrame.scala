package P02_ListsStrings

object P19StringsInFrame {
  def P19ListStringsInFrame(list: List[String]): Unit = {
    val maxLength = list.map(s => s.length).max
    println("*" * (maxLength + 4))
    list.foreach(s => println(f"* $s" + (" " * (maxLength - s.length)) + " *"))
    println("*" * (maxLength + 4))
  }

  def main(args: Array[String]): Unit = {
    println(f"P19 L StringsInFrame: ")
    P19ListStringsInFrame(List("Hello", "World", "in", "a", "frame"))
  }
}
