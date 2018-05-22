package connectFour

import scala.annotation.tailrec

object AI {

  //TODO cały algorytm min-max: makeMove, evaluate, itp.
  val RED: Int = Game.RED
  val YELLOW: Int = Game.YELLOW
  val NEUTRAL: Int = Game.NEUTRAL
  val WIDTH: Int = Game.WIDTH
  val HEIGHT: Int = Game.HEIGHT
  val depth: Int = 5
  val infinity: Long = 1000000000L
  val VALUE4: Long = 1000L
  val VALUE3: Long = 100L
  val VALUE2: Long = 10L
  val VALUE1: Long = 1L
  val BADVALUE4: Long = -10000L
  val BADVALUE3: Long = -1000L
  val BADVALUE2: Long = -10L
  val BADVALUE1: Long = -1L


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

    if (currDepth == 0 || freeSpace == 0) { //if end of searching
      evaluate(board)
    }
    else if (checkWin(board) == RED) { //player won
      if (currDepth >= depth - 3)
        evaluate(board) + 10 * BADVALUE4
      else
        evaluate(board) + currDepth * BADVALUE4
    }
    else if (checkWin(board) == YELLOW) { //AI won
      if (currDepth >= depth - 3)
        evaluate(board) + 100 * currDepth * VALUE4
      else
        evaluate(board) + currDepth * VALUE4
    }
    else { //no one won, continue checking
      if (ifAImoves)
        moveAI(board, currDepth, alpha, beta)
      else
        movePlayer(board, currDepth, alpha, beta)
    }
  }

  def moveAI(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long): Long = {
//    for {
//      i <- 0 to WIDTH if board(i)(0) == NEUTRAL
//    } yield {
//      val newBoard = board
//      newBoard(i).update(gravity(i), YELLOW)
//      val alphaResult = alphabeta(newBoard, false, currDepth - 1, alpha, beta)
//    }
    @tailrec
    def moveAIrec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if(iter < 0 || alpha >= beta)
        alpha
      else {
        val newBoard = board
        newBoard(iter).update(gravity(iter), YELLOW)
        val alphaResult = alphabeta(newBoard, false, currDepth - 1, alpha, beta)
        moveAIrec(board, currDepth, math.max(alpha, alphaResult), beta, iter - 1)
      }
    }

    moveAIrec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  def movePlayer(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long): Long = {
    @tailrec
    def movePlayerRec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if(iter < 0 || alpha >= beta)
        beta
      else {
        val newBoard = board
        newBoard(iter).update(gravity(iter), RED)
        val betaResult = alphabeta(newBoard, true, currDepth - 1, alpha, beta)
        movePlayerRec(board, currDepth, alpha, math.min(beta, betaResult), iter - 1)
      }
    }

    movePlayerRec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  def evaluate(board: Array[Array[Int]]): Long = {
    //TODO
    0L
  }
}

