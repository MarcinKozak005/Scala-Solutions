class P38_QueenAttack {

}


case class Queen(x: Int, y: Int)

object Queen {
  def create(x: Int, y: Int): Option[Queen] = {
    def isOnChessboard(pos: Int): Boolean = pos >= 0 && pos <= 7

    if (isOnChessboard(x) && isOnChessboard(y)) Some(new Queen(x, y))
    else None
  }
}

object QueenAttack {
  def canAttack(q1: Queen, q2: Queen): Boolean = {
    q1.x == q2.x ||
      q1.y == q2.y ||
      (q1.x - q2.x).abs == (q1.y - q2.y).abs
  }
}