import scala.collection.mutable
import scala.collection.mutable.Map

class P05_GradeSchool {

  type DB = mutable.Map[Int, Seq[String]]

  val roster: DB = mutable.Map[Int,Seq[String]]()
  
  def add(name: String, g: Int): Unit = {
    if (roster.keySet.contains(g)) roster(g) = roster(g) :+ name
    else roster(g) = Seq(name)
  }

  def db: DB = roster

  def grade(g: Int): Seq[String] =
  if (roster.keySet.contains(g)) roster(g)
  else Seq()

  def sorted: DB = {
    val m = mutable.Map[Int,Seq[String]]()
    for (k <- roster.keys.toSeq.sorted){
      m(k) = roster(k).sorted
    }
    m
  }
}

