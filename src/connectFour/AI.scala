package connectFour

object AI {

  //TODO cały algorytm min-max: makeMove, evaluate, itp.
  val RED = Game.RED
  val YELLOW = Game.YELLOW
  val NEUTRAL = Game.NEUTRAL
  val WIDTH = Game.WIDTH
  val HEIGHT = Game.HEIGHT
  val depth = 5
  val infinity = 1000000000L

  def checkWin() = Game.checkWin()

  def gravity(column: Int) = Game.gravity(column)

  def makeRandomMove(): Int = {

    val r = scala.util.Random
    r.nextInt(7)
  }

  def makeMove(): Int = {
    val alpha = -infinity

    val results = for {
      i <- 0 to WIDTH if Game.board(i)(0) == NEUTRAL
    } yield {
      val board = Game.board
      board(i).update(gravity(i), YELLOW)
      alphabeta(board, false, depth - 1, alpha, infinity)
    }

    results.indexOf(results.max)
  }

  def alphabeta(board: Array[Array[Int]], ifAImoves: Boolean, currDepth: Int, alpha: Long, beta: Long): Long = {

    10L
  }

  def evaluate(board: Array[Array[Int]]): Long = {
    //TODO
    0L
  }
}

