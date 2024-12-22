package Y2024

import scala.io.{BufferedSource, Source}
import scala.util.Using


object D01 {

  def P01(): Unit = processFile { file =>
    val lines = file.getLines()
    val initialLists = ListPair(List[Int](), List[Int]())
    val finalLists = lines.foldLeft(initialLists) {
      (listsPair, line) => splitLineP01(line, listsPair.first, listsPair.second)
    }
    finalLists.first.sorted
      .zip(finalLists.second.sorted)
      .map { case (first, second) => Math.abs(first - second) }
      .sum
  }

  def P02(): Unit = processFile { file =>
    val lines = file.getLines()
    val initialMaps = MapPair(Map[Int, Int](), Map[Int, Int]())
    val finalMaps = lines.foldLeft(initialMaps) {
      (mapsPair, line) => splitLineP02(line, mapsPair.first, mapsPair.second)
    }
    finalMaps.first.map { case (id, count) => count * id * finalMaps.second.getOrElse(id, 0) }.sum
  }

  private case class ListPair(first: List[Int], second: List[Int])

  private case class MapPair(first: Map[Int, Int], second: Map[Int, Int])

  private def processFile(processFunction: BufferedSource => Int): Unit =
    Using(Source.fromFile("src/main/resources/2024/D01.txt")) { file =>
      processFunction(file)
    }.fold(
      e => println(e.getMessage),
      result => println(result)
    )

  private def splitLineP01(line: String, first: List[Int], second: List[Int]): ListPair = {
    val separatedNumbers = line.split("\\s+")
    ListPair(separatedNumbers(0).toInt :: first, separatedNumbers(1).toInt :: second)
  }

  private def splitLineP02(line: String, first: Map[Int, Int], second: Map[Int, Int]): MapPair = {
    val separatedNumbers = line.split("\\s+")
    MapPair(incrementCount(first, separatedNumbers(0).toInt), incrementCount(second, separatedNumbers(1).toInt))
  }

  private def incrementCount(map: Map[Int, Int], id: Int): Map[Int, Int] = map.get(id) match {
    case Some(count) => map.updated(id, count + 1)
    case None => map.updated(id, 1)
  }


  def main(args: Array[String]): Unit = {
    P01()
    P02()
  }

}
