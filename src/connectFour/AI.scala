package connectFour

import scala.annotation.tailrec

object AI {

  //TODO cały algorytm min-max: makeMove, evaluate, itp.
  val RED: Int = Game.RED
  val YELLOW: Int = Game.YELLOW
  val NEUTRAL: Int = Game.NEUTRAL
  val WIDTH: Int = Game.WIDTH
  val HEIGHT: Int = Game.HEIGHT
  val tab: Array[Array[Int]] = Game.board
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

  def horizontal1(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 3) {
      if (tab(i + 1)(j) == actColor && tab(i + 2)(j) == actColor && tab(i + 3)(j) == actColor)
        value + {
          if (actColor == YELLOW) VALUE4 else BADVALUE4
        } //x-1-1-1
      else if (tab(i + 1)(j) == actColor && tab(i + 2)(j) == actColor && tab(i + 3)(j) == 0)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //x-1-1-0
      else if (tab(i + 1)(j) == actColor && tab(i + 2)(j) == 0 && tab(i + 3)(j) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //x-1-0-0
      else if (tab(i + 1)(j) == 0 && tab(i + 2)(j) != oppColor && tab(i + 3)(j) != oppColor) {
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        }
      } //x-0-0-0
      else value
    }
    else value
  }

  def upRight1(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if ((i < WIDTH - 3) && j > 2) {
      if (tab(i + 1)(j - 1) == actColor && tab(i + 2)(j - 2) == actColor && tab(i + 3)(j - 3) == actColor)
        value + {
          if (actColor == YELLOW) VALUE4 else BADVALUE4
        } //x-1-1-1
      else if (tab(i + 1)(j - 1) == actColor && tab(i + 2)(j - 2) == actColor && tab(i + 3)(j - 3) == 0)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //x-1-1-0
      else if (tab(i + 1)(j - 1) == actColor && tab(i + 2)(j - 2) == 0 && tab(i + 3)(j - 3) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //x-1-0-0
      else if (tab(i + 1)(j - 1) == 0 && tab(i + 2)(j - 2) != oppColor && tab(i + 3)(j - 3) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //x-0-0-0
      else value
    }
    else value
  }

  def downRight1(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if ((i < WIDTH - 3) && j < HEIGHT - 3) //down-right x-0-0-0
    {
      if (tab(i + 3)(j + 3) != oppColor && tab(i + 2)(j + 2) != oppColor && tab(i + 1)(j + 1) == 0)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //x-0-0-0
      else value
    }
    else value
  }


  def horizontal2(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i < WIDTH-2 && i > 0) //horizontal 0-x-?-?
    {
      if(tab(i-1)(j) == 0 && tab(i+1)(j) == actColor && tab(i+2)(j) == actColor)
        value + {if(actColor == YELLOW) VALUE3 else BADVALUE3} //0-x-1-1
      else if(tab(i-1)(j) == 0 && tab(i+1)(j) == actColor && tab(i+2)(j) == 0)
        value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //0-x-1-0
      else if(tab(i-1)(j) == 0 && tab(i+1)(j) == 0 && tab(i+2)(j) != oppColor)
        value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-x-0-0
      else value
    }
    else value
  }

  def upRight2(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i < WIDTH-2 && i > 0) //horizontal 0-x-?-?
    {
      if(j > 1 && j < HEIGHT-1) //up-right 0-x-?-?
      {
        if(tab(i-1)(j+1) == 0 && tab(i+1)(j-1) == actColor && tab(i+2)(j-2) == actColor)
          value + {if(actColor == YELLOW) VALUE3 else BADVALUE3} //0-x-1-1
        else if(tab(i-1)(j+1) == 0 && tab(i+1)(j-1) == actColor && tab(i+2)(j-2) == 0)
          value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //0-x-1-0
        else if(tab(i-1)(j+1) == 0 && tab(i+1)(j-1) == 0 && tab(i+2)(j-2) != oppColor)
          value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-x-0-0
        else value
      }
       else value
    }
    else value
  }

  def downRight2(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 2 && i > 0) //horizontal 0-x-?-?
    {
      if (j > 0 && j < HEIGHT - 2) //down-right ?-x-0-0
      {
        if (tab(i + 2)(j + 2) != oppColor && tab(i + 1)(j + 1) == 0 && tab(i - 1)(j - 1) == actColor)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //1-x-0-0
        else if (tab(i + 2)(j + 2) != oppColor && tab(i + 1)(j + 1) == 0 && tab(i - 1)(j - 1) == 0)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //0-x-0-0
        else value
      }
      else value
    }
    else value
  }

  def horizontal3(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i < WIDTH-1 && i > 1) //horizontal 0-0-x-?
    {
      if(tab(i-2)(j) != oppColor && tab(i-1)(j) == 0 && tab(i+1)(j) == actColor)
        value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //0-0-x-1
      else if(tab(i-2)(j) != oppColor && tab(i-1)(j) == 0 && tab(i+1)(j) == 0)
        value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-0-x-0
      else value
    }
    else value
  }

  def upRight3(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i < WIDTH-1 && i > 1) //horizontal 0-0-x-?
    {
      if(j > 0 && j < HEIGHT-2) //up-right 0-0-x-?
      {
        if(tab(i-2)(j+2) != oppColor && tab(i-1)(j+1) == 0 && tab(i+1)(j-1) == actColor)
          value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //0-0-x-1
        else if(tab(i-2)(j+2) != oppColor && tab(i-1)(j+1) == 0 && tab(i+1)(j-1) == 0)
          value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-0-x-0
        else value
      }
      else value
    }
    else value
  }

  def downRight3(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i < WIDTH-1 && i > 1) //horizontal 0-0-x-?
    {
      if(j > 1 && j < HEIGHT-1) //down-right ?-?-x-0
      {
        if(tab(i+1)(j+1) == 0 && tab(i-1)(j-1) == actColor && tab(i-2)(j-2) == actColor)
          value + {if(actColor == YELLOW) VALUE3 else BADVALUE3} //0-x-1-1
        else if(tab(i+1)(j+1) == 0 && tab(i-1)(j-1) == actColor && tab(i-2)(j-2) == 0)
          value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //0-x-1-0
        else if(tab(i+1)(j+1) == 0 && tab(i-1)(j-1) == 0 && tab(i-2)(j-2) != oppColor)
          value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-x-0-0
        else value
      }
      else value
    }
    else value
  }


  def horizontal4(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i > 2) //horizontal 0-0-0-x
    {
      if(tab(i-3)(j) != oppColor && tab(i-2)(j) != oppColor && tab(i-1)(j) == 0)
        value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //0-0-0-x
      else value
    }
    else value
  }

  def upRight4(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i > 2) //horizontal 0-0-0-x
    {
      if(j < HEIGHT-3) //up-right 0-0-0-x
      {
        if(tab(i-3)(j+3) != oppColor && tab(i-2)(j+2) != oppColor && tab(i-1)(j+1) == 0)
          value + {if(actColor == YELLOW) VALUE1 else BADVALUE1}
        else value
      }
      else value
    }
    else value
  }

  def downRight4(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(i > 2) //horizontal 0-0-0-x
    {
      if(j > 2) //down-right ?-?-?-x
      {
        if(tab(i-1)(j-1) == actColor && tab(i-2)(j-2) == actColor && tab(i-3)(j-3) == actColor)
          value + {if(actColor == YELLOW) VALUE4 else BADVALUE4} //x-1-1-1
        else if(tab(i-1)(j-1) == actColor && tab(i-2)(j-2) == actColor && tab(i-3)(j-3) == 0)
          value + {if(actColor == YELLOW) VALUE3 else BADVALUE3} //x-1-1-0
        else if(tab(i-1)(j-1) == actColor && tab(i-2)(j-2) == 0 && tab(i-3)(j-3) != oppColor)
          value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //x-1-0-0
        else if(tab(i-1)(j-1) == 0 && tab(i-2)(j-2) != oppColor && tab(i-3)(j-3) != oppColor)
          value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //x-0-0-0
        else value
      }
      else value
    }
    else value
  }

  def vertical(value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if(j > 2) //vertical
    {
      if(tab(i)(j-1) == 0)
        value + {if(actColor == YELLOW) VALUE1 else BADVALUE1} //1s up
      else if(tab(i)(j-1) == actColor && tab(i)(j-2) == 0)
        value + {if(actColor == YELLOW) VALUE2 else BADVALUE2} //2s up
      else if(tab(i)(j-1) == actColor && tab(i)(j-2) == actColor && tab(i)(j-3) == 0)
        value + {if(actColor == YELLOW) VALUE3 else BADVALUE3} //3s up
      else if(tab(i)(j-1) == actColor && tab(i)(j-2) == actColor && tab(i)(j-3) == actColor)
        value + {if(actColor == YELLOW) VALUE4 else BADVALUE4} //4s up
      else value
    }
    else value
  }

  def evaluate(board: Array[Array[Int]]): Long = {
    val value: Long = 0L
    val forValue =
      for {i <- 0 until WIDTH
         j <- 0 until HEIGHT
         if(tab(i)(j) != NEUTRAL)} yield {

      val actColor = tab(i)(j)
      val oppColor = if (actColor == RED) YELLOW else RED

      val horizontalValue1 = horizontal1(value, i, j, actColor, oppColor)
      val upRightValue1 = upRight1(horizontalValue1, i, j, actColor, oppColor)
      val downRightValue1 = downRight1(upRightValue1, i, j, actColor, oppColor)

      val horizontalValue2 = horizontal2(downRightValue1, i, j, actColor, oppColor)
      val upRightValue2 = upRight2(horizontalValue2, i, j, actColor, oppColor)
      val downRightValue2 = downRight2(upRightValue2, i, j, actColor, oppColor)

      val horizontalValue3 = horizontal3(downRightValue2, i, j, actColor, oppColor)
      val upRightValue3 = upRight3(horizontalValue3, i, j, actColor, oppColor)
      val downRightValue3 = downRight3(upRightValue3, i, j, actColor, oppColor)

      val horizontalValue4 = horizontal4(downRightValue3, i, j, actColor, oppColor)
      val upRightValue4 = upRight4(horizontalValue4, i, j, actColor, oppColor)
      val downRightValue4 = downRight4(upRightValue4, i, j, actColor, oppColor)

      val verticalValue = vertical(downRightValue4, i, j, actColor, oppColor)
    }
  }


}

