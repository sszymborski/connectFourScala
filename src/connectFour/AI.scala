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

  def makeMove() : Int = {

    val r = scala.util.Random
    r.nextInt(7)
  }

  def makeMove(board: Array[Array[Int]]) : Int = {
    val alpha = -1000000000L

    for {
      i <- 0 to WIDTH if board(i)(0) == NEUTRAL
    } yield {
      val newBoard = board
      newBoard(i).update(gravity(i), YELLOW) //TODO uzyskać index przez jakiś map i sum
    }
    0
  }
}

