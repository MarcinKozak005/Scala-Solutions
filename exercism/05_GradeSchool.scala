import scala.collection.mutable.Map

class School {
  type DB = Map[Int, Seq[String]]

  val roster = Map[Int,Seq[String]]()
  
  def add(name: String, g: Int) = {
    if (roster.keySet.exists(_ == g)) roster(g) = roster(g) :+ name
    else roster(g) = Seq(name)
  }

  def db: DB = roster

  def grade(g: Int): Seq[String] =
  if (roster.keySet.exists(_ == g)) roster(g)
  else Seq()

  def sorted: DB = {
    val m = Map[Int,Seq[String]]()
    for (k <- roster.keys.toSeq.sorted){
      m(k) = roster(k).sorted
    }
    m
  }
}

