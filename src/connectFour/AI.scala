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
  val VALUE4 = 1000
  val VALUE3 = 100
  val VALUE2 = 10
  val VALUE1 = 1
  val BADVALUE4 = -10000
  val BADVALUE3 = -1000
  val BADVALUE2 = -10
  val BADVALUE1 = -1


  def checkWin(table: Array[Array[Int]]) = Game.checkWin(table)

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
    val freeSpace = {
      for {
        i <- 0 to WIDTH
      } yield board(i)(0)
    }.map {
      case NEUTRAL => 1
      case _ => 0
    }.sum

    if(currDepth == 0 || freeSpace == 0){ //if end of searching
      evaluate(board)
    }
    else if(checkWin(board) == RED){ //player won
      if(currDepth >= depth - 3)
        evaluate(board) + 10 * BADVALUE4
      else
        evaluate(board) + currDepth * BADVALUE4
    }
    else if(checkWin(board) == YELLOW){ //AI won
      if(currDepth >= depth - 3)
        evaluate(board) + 100 * currDepth * VALUE4
      else
        evaluate(board) + currDepth * VALUE4
    }//TODO else

    10L
  }

  def evaluate(board: Array[Array[Int]]): Long = {
    //TODO
    0L
  }
}

