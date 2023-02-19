package Y2015

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object D03 {

  def P01Immutable(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D03.txt")
    val line = data.getLines().next()
    processChar((0, 0), Set((0, 0)), line).size
  }

  @tailrec
  def processChar(currentPosition: (Int, Int), set: Set[(Int, Int)], str: String): Set[(Int, Int)] = {
    if (str.isEmpty) set
    else {
      val newPosition = str match {
        case s"^$_" => (currentPosition._1, currentPosition._2 + 1)
        case s">$_" => (currentPosition._1 + 1, currentPosition._2)
        case s"v$_" => (currentPosition._1, currentPosition._2 - 1)
        case s"<$_" => (currentPosition._1 - 1, currentPosition._2)
      }
      processChar(newPosition, set.incl(newPosition), str.slice(1, str.length))
    }
  }

  def P01Mutable(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D03.txt")
    val line = data.getLines().next()
    var currentPosition = (0, 0)
    val set = mutable.Set((0, 0))
    line.foreach(c => set.addOne({
      c match {
        case '^' => currentPosition = (currentPosition._1, currentPosition._2 + 1)
        case '>' => currentPosition = (currentPosition._1 + 1, currentPosition._2)
        case 'v' => currentPosition = (currentPosition._1, currentPosition._2 - 1)
        case '<' => currentPosition = (currentPosition._1 - 1, currentPosition._2)
      }
      currentPosition
    }))
    set.size
  }

  def P02Immutable(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D03.txt")
    val line = data.getLines().next()
    val santaInstructions = line.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString
    val roboSantaInstructions = line.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    val startingPos = (0, 0)
    processChar(
      startingPos,
      processChar(startingPos, Set(startingPos), santaInstructions),
      roboSantaInstructions
    ).size
  }

  def P02Mutable(): Int = {
    val data = Source.fromFile("src/main/resources/2015/D03.txt")
    val line = data.getLines().next()
    var currentPosition = (0, 0)
    val set = mutable.Set((0, 0))
    val santaInstructions = line.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString
    val roboSantaInstructions = line.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    santaInstructions.foreach(c => set.addOne({
      c match {
        case '^' => currentPosition = (currentPosition._1, currentPosition._2 + 1)
        case '>' => currentPosition = (currentPosition._1 + 1, currentPosition._2)
        case 'v' => currentPosition = (currentPosition._1, currentPosition._2 - 1)
        case '<' => currentPosition = (currentPosition._1 - 1, currentPosition._2)
      }
      currentPosition
    }))
    currentPosition = (0, 0)
    roboSantaInstructions.foreach(c => set.addOne({
      c match {
        case '^' => currentPosition = (currentPosition._1, currentPosition._2 + 1)
        case '>' => currentPosition = (currentPosition._1 + 1, currentPosition._2)
        case 'v' => currentPosition = (currentPosition._1, currentPosition._2 - 1)
        case '<' => currentPosition = (currentPosition._1 - 1, currentPosition._2)
      }
      currentPosition
    }))
    set.size
  }

  def main(args: Array[String]): Unit = {
    println(P01Immutable())
    println(P01Mutable())
    println(P02Immutable())
    println(P02Mutable())
  }

}
