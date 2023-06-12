package P01_Elementary

object P07MultiplicationTable extends App {
  val r = 1 to 12
  for (i <- r) {
    for (j <- r) print(f"${i * j}%3d ")
    println()
  }
}
