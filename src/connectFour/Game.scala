package connectFour

import scala.util.control.Breaks._

/**
  * Main object of the application.
  *
  */
object Game extends App {
  val RED: Int = 1
  val YELLOW: Int = 2
  val WIDTH: Int = 7
  val HEIGHT: Int = 6
  val NEUTRAL: Int = 0
  val board = Array.ofDim[Int](WIDTH, HEIGHT)
  val whoPlays = true

  /**
    * Function checking if there is a winning vertical alignment of pucks.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param color color of puck we started search from
    * @param endCheck boolean parameter informing whether to display information about win or not
    * @return true if there is a win, otherwise false
    */
  def checkVertically(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until WIDTH
                      j <- (HEIGHT - 1) until (HEIGHT - 4) by -1
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if ((actual == table(i)(j - 1)) && (actual == table(i)(j - 2)) && (actual == table(i)(j - 3))) {
          if (endCheck)
            Gui.showWinMessage(i, j, i, j - 1, i, j - 2, i, j - 3)
          1
        }
        else
          0
      }
      else
        0
    }
    result.sum > 0
  }

  /**
    * Function checking if there is a winning horizontal alignment of pucks.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param color color of puck we started search from
    * @param endCheck boolean parameter informing whether to display information about win or not
    * @return true if there is a win, otherwise false
    */
  def checkHorizontally(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- 0 until HEIGHT
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j) && actual == table(i + 2)(j) && actual == table(i + 3)(j)) {
          if (endCheck)
            Gui.showWinMessage(i, j, i + 1, j, i + 2, j, i + 3, j)
          1
        }
        else
          0
      }
      else
        0
    }
    result.sum > 0
  }

  /**
    * Function checking if there is a winning diagonal-upward alignment of pucks.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param color color of puck we started search from
    * @param endCheck boolean parameter informing whether to display information about win or not
    * @return true if there is a win, otherwise false
    */
  def checkDiagonallyUpRight(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- (HEIGHT - 1) until (HEIGHT - 4) by -1
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j - 1) && actual == table(i + 2)(j - 2) && actual == table(i + 3)(j - 3)) {
          if (endCheck)
            Gui.showWinMessage(i, j, i + 1, j - 1, i + 2, j - 2, i + 3, j - 3)
          1
        }
        else
          0
      }
      else
        0
    }
    result.sum > 0
  }

  /**
    * Function checking if there is a winning diagonal-downward alignment of pucks.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param color color of puck we started search from
    * @param endCheck boolean parameter informing whether to display information about win or not
    * @return true if there is a win, otherwise false
    */
  def checkDiagonallyDownRight(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- 0 until (HEIGHT - 3)
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j + 1) && actual == table(i + 2)(j + 2) && actual == table(i + 3)(j + 3)) {
          if (endCheck)
            Gui.showWinMessage(i, j, i + 1, j + 1, i + 2, j + 1, i + 3, j + 3)
          1
        }
        else
          0
      }
      else
        0
    }
    result.sum > 0
  }

  /**
    * Function checking if there is a winning alignment of pucks.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param endCheck boolean parameter informing whether to display information about win or not
    * @return color of winning pucks or value of empty field in case of no winning situation
    */
  def checkWin(table: Array[Array[Int]], endCheck: Boolean = false): Int = {
    if (checkVertically(table, RED, endCheck) || checkHorizontally(table, RED, endCheck)
      || checkDiagonallyDownRight(table, RED, endCheck) || checkDiagonallyUpRight(table, RED, endCheck))
      RED
    else if (checkVertically(table, YELLOW, endCheck) || checkHorizontally(table, YELLOW, endCheck)
      || checkDiagonallyDownRight(table, YELLOW, endCheck) || checkDiagonallyUpRight(table, YELLOW, endCheck))
      YELLOW
    else
      NEUTRAL
  }

  /**
    * Function calculating the lowest row in specified column where puck can be put in.
    *
    * @param table two-dimensional array containing current layout of pucks
    * @param column column we want to put puck in
    * @return
    */
  def gravity(table: Array[Array[Int]], column: Int): Int = {
    table(column).map {
      case NEUTRAL => 1
      case _ => 0
    }.sum - 1
  }

  /**
    * Function responsible for getting input fro player about a column he wants to put his puck in and for putting player's puck in that column.
    *
    */
  def makeMove(): Unit = {
    val column = Gui.getInputColumn
    val row = gravity(board, column)
    if (row < 0)
      makeMove()
    else
      board(column)(row) = RED
  }

  /**
    * Function getting decision about next move from AI and making this move.
    *
    * @param ai instance of AI case class
    */
  def AImakeMove(ai: AI): Unit = {
    val column = ai.makeMove()
    val row = gravity(board, column)
    if (row < 0)
      AImakeMove(ai)
    else
      board(column)(row) = YELLOW
  }

  val ai: AI = AI(Gui.getInputDepth)

  Gui.display()

  breakable {
    for (_ <- 1 to 21) {
      makeMove()
      Gui.display()
      if (checkWin(board, endCheck = true) != NEUTRAL) break
      AImakeMove(ai)
      Gui.display()
      if (checkWin(board, endCheck = true) != NEUTRAL) break
    }
    Gui.showDrawMessage()
  }
}


