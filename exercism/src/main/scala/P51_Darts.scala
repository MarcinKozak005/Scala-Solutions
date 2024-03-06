import scala.math.sqrt

class P51_Darts {

}


object Darts {
  def score(x: Double, y: Double): Int = {
    val distance = sqrt(x * x + y * y)
    if (distance <= 1.0) 10
    else if (distance <= 5.0) 5
    else if (distance <= 10.0) 1
    else 0
  }
}