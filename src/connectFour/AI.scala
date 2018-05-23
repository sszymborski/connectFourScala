package connectFour

import scala.annotation.tailrec

object AI {
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
  def gravity(board: Array[Array[Int]], column: Int) = Game.gravity(board, column)

  def makeMove(): Int = {
    val alpha = -infinity
    val results = for {
      i <- 0 until WIDTH if Game.board(i)(0) == NEUTRAL
    } yield {
      val table = Game.board.map(_.clone())
      table(i).update(gravity(table, i), YELLOW)
      val res = alphabeta(table, false, depth - 1, alpha, infinity)
      print(res + "\t")
      res
    }

    val res = results.indexOf(results.max)
    println(res)
    println(results)
    res
  }

  def alphabeta(board: Array[Array[Int]], ifAImoves: Boolean, currDepth: Int, alpha: Long, beta: Long): Long = {
    val freeSpace = {
      for {
        i <- 0 until WIDTH
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
    @tailrec
    def moveAIrec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if (iter < 0 || alpha >= beta || board(iter)(0) != NEUTRAL)
        alpha
      else {
        val newBoard = board.map(_.clone())
        newBoard(iter).update(gravity(newBoard, iter), YELLOW)
        val alphaResult = alphabeta(newBoard, false, currDepth - 1, alpha, beta)
        moveAIrec(board, currDepth, math.max(alpha, alphaResult), beta, iter - 1)
      }
    }
    moveAIrec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  def movePlayer(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long): Long = {
    @tailrec
    def movePlayerRec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if (iter < 0 || alpha >= beta || board(iter)(0) != NEUTRAL)
        beta
      else {
        val newBoard = board.map(_.clone())
        newBoard(iter).update(gravity(newBoard, iter), RED)
        val betaResult = alphabeta(newBoard, true, currDepth - 1, alpha, beta)
        movePlayerRec(board, currDepth, alpha, math.min(beta, betaResult), iter - 1)
      }
    }
    movePlayerRec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  def horizontal1(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 3) {
      if (board(i + 1)(j) == actColor && board(i + 2)(j) == actColor && board(i + 3)(j) == actColor)
        value + {
          if (actColor == YELLOW) VALUE4 else BADVALUE4
        } //x-1-1-1
      else if (board(i + 1)(j) == actColor && board(i + 2)(j) == actColor && board(i + 3)(j) == 0)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //x-1-1-0
      else if (board(i + 1)(j) == actColor && board(i + 2)(j) == 0 && board(i + 3)(j) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //x-1-0-0
      else if (board(i + 1)(j) == 0 && board(i + 2)(j) != oppColor && board(i + 3)(j) != oppColor) {
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        }
      } //x-0-0-0
      else value
    }
    else value
  }

  def upRight1(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if ((i < WIDTH - 3) && j > 2) {
      if (board(i + 1)(j - 1) == actColor && board(i + 2)(j - 2) == actColor && board(i + 3)(j - 3) == actColor)
        value + {
          if (actColor == YELLOW) VALUE4 else BADVALUE4
        } //x-1-1-1
      else if (board(i + 1)(j - 1) == actColor && board(i + 2)(j - 2) == actColor && board(i + 3)(j - 3) == 0)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //x-1-1-0
      else if (board(i + 1)(j - 1) == actColor && board(i + 2)(j - 2) == 0 && board(i + 3)(j - 3) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //x-1-0-0
      else if (board(i + 1)(j - 1) == 0 && board(i + 2)(j - 2) != oppColor && board(i + 3)(j - 3) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //x-0-0-0
      else value
    }
    else value
  }

  def downRight1(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if ((i < WIDTH - 3) && j < HEIGHT - 3) //down-right x-0-0-0
    {
      if (board(i + 3)(j + 3) != oppColor && board(i + 2)(j + 2) != oppColor && board(i + 1)(j + 1) == 0)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //x-0-0-0
      else value
    }
    else value
  }

  def horizontal2(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 2 && i > 0) //horizontal 0-x-?-?
    {
      if (board(i - 1)(j) == 0 && board(i + 1)(j) == actColor && board(i + 2)(j) == actColor)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //0-x-1-1
      else if (board(i - 1)(j) == 0 && board(i + 1)(j) == actColor && board(i + 2)(j) == 0)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //0-x-1-0
      else if (board(i - 1)(j) == 0 && board(i + 1)(j) == 0 && board(i + 2)(j) != oppColor)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //0-x-0-0
      else value
    }
    else value
  }

  def upRight2(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 2 && i > 0) //horizontal 0-x-?-?
    {
      if (j > 1 && j < HEIGHT - 1) //up-right 0-x-?-?
      {
        if (board(i - 1)(j + 1) == 0 && board(i + 1)(j - 1) == actColor && board(i + 2)(j - 2) == actColor)
          value + {
            if (actColor == YELLOW) VALUE3 else BADVALUE3
          } //0-x-1-1
        else if (board(i - 1)(j + 1) == 0 && board(i + 1)(j - 1) == actColor && board(i + 2)(j - 2) == 0)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //0-x-1-0
        else if (board(i - 1)(j + 1) == 0 && board(i + 1)(j - 1) == 0 && board(i + 2)(j - 2) != oppColor)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //0-x-0-0
        else value
      }
      else value
    }
    else value
  }

  def downRight2(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 2 && i > 0) //horizontal 0-x-?-?
    {
      if (j > 0 && j < HEIGHT - 2) //down-right ?-x-0-0
      {
        if (board(i + 2)(j + 2) != oppColor && board(i + 1)(j + 1) == 0 && board(i - 1)(j - 1) == actColor)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //1-x-0-0
        else if (board(i + 2)(j + 2) != oppColor && board(i + 1)(j + 1) == 0 && board(i - 1)(j - 1) == 0)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //0-x-0-0
        else value
      }
      else value
    }
    else value
  }

  def horizontal3(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 1 && i > 1) //horizontal 0-0-x-?
    {
      if (board(i - 2)(j) != oppColor && board(i - 1)(j) == 0 && board(i + 1)(j) == actColor)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //0-0-x-1
      else if (board(i - 2)(j) != oppColor && board(i - 1)(j) == 0 && board(i + 1)(j) == 0)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //0-0-x-0
      else value
    }
    else value
  }

  def upRight3(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 1 && i > 1) //horizontal 0-0-x-?
    {
      if (j > 0 && j < HEIGHT - 2) //up-right 0-0-x-?
      {
        if (board(i - 2)(j + 2) != oppColor && board(i - 1)(j + 1) == 0 && board(i + 1)(j - 1) == actColor)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //0-0-x-1
        else if (board(i - 2)(j + 2) != oppColor && board(i - 1)(j + 1) == 0 && board(i + 1)(j - 1) == 0)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //0-0-x-0
        else value
      }
      else value
    }
    else value
  }

  def downRight3(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i < WIDTH - 1 && i > 1) //horizontal 0-0-x-?
    {
      if (j > 1 && j < HEIGHT - 1) //down-right ?-?-x-0
      {
        if (board(i + 1)(j + 1) == 0 && board(i - 1)(j - 1) == actColor && board(i - 2)(j - 2) == actColor)
          value + {
            if (actColor == YELLOW) VALUE3 else BADVALUE3
          } //0-x-1-1
        else if (board(i + 1)(j + 1) == 0 && board(i - 1)(j - 1) == actColor && board(i - 2)(j - 2) == 0)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //0-x-1-0
        else if (board(i + 1)(j + 1) == 0 && board(i - 1)(j - 1) == 0 && board(i - 2)(j - 2) != oppColor)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //0-x-0-0
        else value
      }
      else value
    }
    else value
  }


  def horizontal4(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i > 2) //horizontal 0-0-0-x
    {
      if (board(i - 3)(j) != oppColor && board(i - 2)(j) != oppColor && board(i - 1)(j) == 0)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //0-0-0-x
      else value
    }
    else value
  }

  def upRight4(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i > 2) //horizontal 0-0-0-x
    {
      if (j < HEIGHT - 3) //up-right 0-0-0-x
      {
        if (board(i - 3)(j + 3) != oppColor && board(i - 2)(j + 2) != oppColor && board(i - 1)(j + 1) == 0)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          }
        else value
      }
      else value
    }
    else value
  }

  def downRight4(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (i > 2) //horizontal 0-0-0-x
    {
      if (j > 2) //down-right ?-?-?-x
      {
        if (board(i - 1)(j - 1) == actColor && board(i - 2)(j - 2) == actColor && board(i - 3)(j - 3) == actColor)
          value + {
            if (actColor == YELLOW) VALUE4 else BADVALUE4
          } //x-1-1-1
        else if (board(i - 1)(j - 1) == actColor && board(i - 2)(j - 2) == actColor && board(i - 3)(j - 3) == 0)
          value + {
            if (actColor == YELLOW) VALUE3 else BADVALUE3
          } //x-1-1-0
        else if (board(i - 1)(j - 1) == actColor && board(i - 2)(j - 2) == 0 && board(i - 3)(j - 3) != oppColor)
          value + {
            if (actColor == YELLOW) VALUE2 else BADVALUE2
          } //x-1-0-0
        else if (board(i - 1)(j - 1) == 0 && board(i - 2)(j - 2) != oppColor && board(i - 3)(j - 3) != oppColor)
          value + {
            if (actColor == YELLOW) VALUE1 else BADVALUE1
          } //x-0-0-0
        else value
      }
      else value
    }
    else value
  }

  def vertical(board: Array[Array[Int]], value: Long, i: Int, j: Int, actColor: Int, oppColor: Int): Long = {
    if (j > 2) //vertical
    {
      if (board(i)(j - 1) == 0)
        value + {
          if (actColor == YELLOW) VALUE1 else BADVALUE1
        } //1s up
      else if (board(i)(j - 1) == actColor && board(i)(j - 2) == 0)
        value + {
          if (actColor == YELLOW) VALUE2 else BADVALUE2
        } //2s up
      else if (board(i)(j - 1) == actColor && board(i)(j - 2) == actColor && board(i)(j - 3) == 0)
        value + {
          if (actColor == YELLOW) VALUE3 else BADVALUE3
        } //3s up
      else if (board(i)(j - 1) == actColor && board(i)(j - 2) == actColor && board(i)(j - 3) == actColor)
        value + {
          if (actColor == YELLOW) VALUE4 else BADVALUE4
        } //4s up
      else value
    }
    else value
  }

  def evaluate(board: Array[Array[Int]]): Long = {
    val value: Long = 0L
    @tailrec
    def evaluateRec(board: Array[Array[Int]], value: Long, i: Int): Long = {
      if (i >= WIDTH)
        value
      else {
        @tailrec
        def evaluateRecJ(board: Array[Array[Int]], value: Long, j: Int): Long = {
          if (j < 0 || board(i)(j) == NEUTRAL)
            value
          else {
            val actColor = board(i)(j)
            val oppColor = {
              if (actColor == RED) YELLOW else RED
            }

            val horizontalValue1 = horizontal1(board, value, i, j, actColor, oppColor)
            val upRightValue1 = upRight1(board, horizontalValue1, i, j, actColor, oppColor)
            val downRightValue1 = downRight1(board, upRightValue1, i, j, actColor, oppColor)

            val horizontalValue2 = horizontal2(board, downRightValue1, i, j, actColor, oppColor)
            val upRightValue2 = upRight2(board, horizontalValue2, i, j, actColor, oppColor)
            val downRightValue2 = downRight2(board, upRightValue2, i, j, actColor, oppColor)

            val horizontalValue3 = horizontal3(board, downRightValue2, i, j, actColor, oppColor)
            val upRightValue3 = upRight3(board, horizontalValue3, i, j, actColor, oppColor)
            val downRightValue3 = downRight3(board, upRightValue3, i, j, actColor, oppColor)

            val horizontalValue4 = horizontal4(board, downRightValue3, i, j, actColor, oppColor)
            val upRightValue4 = upRight4(board, horizontalValue4, i, j, actColor, oppColor)
            val downRightValue4 = downRight4(board, upRightValue4, i, j, actColor, oppColor)

            val verticalValue = vertical(board, downRightValue4, i, j, actColor, oppColor)

            evaluateRecJ(board, verticalValue, j - 1)
          }
        }
        val valueJ = evaluateRecJ(board, value, HEIGHT - 1)
        evaluateRec(board, valueJ, i + 1)
      }
    }
    evaluateRec(board, value, 0)
  }
}

