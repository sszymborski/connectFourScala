package connectFour

object AI {

  //TODO cały algorytm min-max: makeMove, evaluate, itp.
  val RED = Game.RED
  val YELLOW = Game.YELLOW
  val NEUTRAL = Game.NEUTRAL
  val WIDTH = Game.WIDTH
  val HEIGHT = Game.HEIGHT
  def checkWin() = Game.checkWin()
  def gravity(column: Int) = Game.gravity(column)

  def makeRandomMove() : Int = {

    val r = scala.util.Random
    r.nextInt(7)
  }

  def makeMove() : Int = {
    val alpha = -1000000000L

    for {
      i <- 0 to WIDTH if Game.board(i)(0) == NEUTRAL
    } yield {
      val newBoard = Game.board
      newBoard(i).update(gravity(i), YELLOW)
    }
    0
  }
}

