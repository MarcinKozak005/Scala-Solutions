class P10_RobotSimulator {}

sealed abstract class Bearing

object Bearing {
  case object North extends Bearing

  case object East extends Bearing

  case object South extends Bearing

  case object West extends Bearing
}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  def turnRight: Robot = Robot(bearing match {
    case Bearing.North => Bearing.East
    case Bearing.East => Bearing.South
    case Bearing.South => Bearing.West
    case Bearing.West => Bearing.North
  }, coordinates)

  def turnLeft: Robot = Robot(bearing match {
    case Bearing.North => Bearing.West
    case Bearing.East => Bearing.North
    case Bearing.South => Bearing.East
    case Bearing.West => Bearing.South
  }, coordinates)

  def advance: Robot = Robot(bearing, bearing match {
    case Bearing.North => (coordinates._1, coordinates._2 + 1)
    case Bearing.East => (coordinates._1 + 1, coordinates._2)
    case Bearing.South => (coordinates._1, coordinates._2 - 1)
    case Bearing.West => (coordinates._1 - 1, coordinates._2)
  })

  def simulate(str: String): Robot = str match {
    case s if s.startsWith("A") => this.advance.simulate(str.tail)
    case s if s.startsWith("R") => this.turnRight.simulate(str.tail)
    case s if s.startsWith("L") => this.turnLeft.simulate(str.tail)
    case _ => this
  }

}
