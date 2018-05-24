package connectFour

import scala.annotation.tailrec

/**
  * Case class with methods implementing all functions of AI player, using min-max algorithm with alpha-beta pruning.
  *
  * @param depth depth of the AI's search tree; it represents number of moves AI is able to predict
  */
case class AI(depth: Int) {
  val RED: Int = Game.RED
  val YELLOW: Int = Game.YELLOW
  val NEUTRAL: Int = Game.NEUTRAL
  val WIDTH: Int = Game.WIDTH
  val HEIGHT: Int = Game.HEIGHT
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

  /**
    * Function starting min-max algorithm in order to choose the best move for AI to make.
    *
    * @return number of column AI wants to place its puck in
    */
  def makeMove(): Int = {
    val alpha = -infinity
    val results = for {
      i <- 0 until WIDTH
    } yield {
      if(Game.board(i)(0) == NEUTRAL) {
        val table = Game.board.map(_.clone())
        table(i).update(gravity(table, i), YELLOW)
        alphabeta(table, ifAImoves = false, depth - 1, alpha, infinity)
      }
      else{
        -infinity
      }
    }
    results.indexOf(results.max)
  }

  /**
    * Main recursive function for min-max algorithm with alpha-beta pruning, called for consecutive tree nodes.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param ifAImoves value informing whether it is AI's move or player's
    * @param currDepth number of tree layers left to consider
    * @param alpha alpha variable of min-max algorithm used in alpha-beta pruning
    * @param beta beta variable of min-max algorithm used in alpha-beta pruning
    * @return number representing evaluated current state of board
    */
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

  /**
    * Function making next AI's predicted move, called by alphabeta() in turns with movePlayer().
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param currDepth number of tree layers left to consider
    * @param alpha alpha variable of min-max algorithm used in alpha-beta pruning
    * @param beta beta variable of min-max algorithm used in alpha-beta pruning
    * @return number representing evaluated current state of board
    */
  def moveAI(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long): Long = {
    @tailrec
    def moveAIrec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if (iter < 0 || alpha >= beta || board(iter)(0) != NEUTRAL)
        alpha
      else {
        val newBoard = board.map(_.clone())
        newBoard(iter).update(gravity(newBoard, iter), YELLOW)
        val alphaResult = alphabeta(newBoard, ifAImoves = false, currDepth - 1, alpha, beta)
        moveAIrec(board, currDepth, math.max(alpha, alphaResult), beta, iter - 1)
      }
    }

    moveAIrec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  /**
    * Function making next player's predicted move, called by alphabeta() in turns with moveAI().
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param currDepth number of tree layers left to consider
    * @param alpha alpha variable of min-max algorithm used in alpha-beta pruning
    * @param beta beta variable of min-max algorithm used in alpha-beta pruning
    * @return number representing evaluated current state of board
    */
  def movePlayer(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long): Long = {
    @tailrec
    def movePlayerRec(board: Array[Array[Int]], currDepth: Int, alpha: Long, beta: Long, iter: Int): Long = {
      if (iter < 0 || alpha >= beta || board(iter)(0) != NEUTRAL)
        beta
      else {
        val newBoard = board.map(_.clone())
        newBoard(iter).update(gravity(newBoard, iter), RED)
        val betaResult = alphabeta(newBoard, ifAImoves = true, currDepth - 1, alpha, beta)
        movePlayerRec(board, currDepth, alpha, math.min(beta, betaResult), iter - 1)
      }
    }

    movePlayerRec(board, currDepth, alpha, beta, WIDTH - 1)
  }

  /**
    * Function summing points for horizontally aligned pucks in a pattern "x - ? - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-upwards aligned pucks in a pattern "x - ? - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-downwards aligned pucks in a pattern "x - ? - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for horizontally aligned pucks in a pattern "? - x - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-upwards aligned pucks in a pattern "? - x - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-downwards aligned pucks in a pattern "? - x - ? - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for horizontally aligned pucks in a pattern "? - ? - x - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-upwards aligned pucks in a pattern "? - ? - x - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-downwards aligned pucks in a pattern "? - ? - x - ?", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for horizontally aligned pucks in a pattern "? - ? - ? - x", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-upwards aligned pucks in a pattern "? - ? - ? - x", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for diagonally-downwards aligned pucks in a pattern "? - ? - ? - x", where "x" means currently considered puck and "?" neighbouring aligned fields.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Function summing points for vertically aligned pucks.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @param value sum of points counted so far
    * @param i number of column of currently considered puck
    * @param j number of row of currently considered puck
    * @param actColor color of currently considered puck
    * @param oppColor color of actColor puck's opponent
    * @return sum of value parameter and points for currently considered alignment of pucks
    */
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

  /**
    * Heuristic function calculating numerical value of current layout of pucks.
    *
    * @param board two-dimensional array containing current layout of pucks
    * @return number representing evaluated current state of board
    */
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
            val horizontalValue1 = horizontal1(board, value, i, j, actColor, oppColor)            //x-?-?-?
            val upRightValue1 = upRight1(board, horizontalValue1, i, j, actColor, oppColor)       //x-?-?-?
            val downRightValue1 = downRight1(board, upRightValue1, i, j, actColor, oppColor)      //x-?-?-?

            val horizontalValue2 = horizontal2(board, downRightValue1, i, j, actColor, oppColor)  //?-x-?-?
            val upRightValue2 = upRight2(board, horizontalValue2, i, j, actColor, oppColor)       //?-x-?-?
            val downRightValue2 = downRight2(board, upRightValue2, i, j, actColor, oppColor)      //?-x-?-?

            val horizontalValue3 = horizontal3(board, downRightValue2, i, j, actColor, oppColor)  //?-?-x-?
            val upRightValue3 = upRight3(board, horizontalValue3, i, j, actColor, oppColor)       //?-?-x-?
            val downRightValue3 = downRight3(board, upRightValue3, i, j, actColor, oppColor)      //?-?-x-?

            val horizontalValue4 = horizontal4(board, downRightValue3, i, j, actColor, oppColor)  //?-?-?-x
            val upRightValue4 = upRight4(board, horizontalValue4, i, j, actColor, oppColor)       //?-?-?-x
            val downRightValue4 = downRight4(board, upRightValue4, i, j, actColor, oppColor)      //?-?-?-x

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

