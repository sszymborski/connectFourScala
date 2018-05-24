package connectFour

import scala.util.control.Breaks._

object Game extends App {
  val RED: Int = 1
  val YELLOW: Int = 2
  val WIDTH: Int = 7
  val HEIGHT: Int = 6
  val NEUTRAL: Int = 0
  val board = Array.ofDim[Int](WIDTH, HEIGHT)
  val whoPlays = true

  def checkVertically(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until WIDTH
                      j <- (HEIGHT - 1) until (HEIGHT - 4) by -1
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if ((actual == table(i)(j - 1)) && (actual == table(i)(j - 2)) && (actual == table(i)(j - 3))) {
          if (endCheck)
            println("Win by " + i + "x" + j + ", " + i + "x" + (j - 1) + ", " + i + "x" + (j - 2) + ", " + i + "x" + (j - 3) + ", ")
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

  def checkHorizontally(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- 0 until HEIGHT
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j) && actual == table(i + 2)(j) && actual == table(i + 3)(j)) {
          if (endCheck)
            println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + j + ", " + (i + 2) + "x" + j + ", " + (i + 3) + "x" + j + ", ")
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

  def checkDiagonallyUpRight(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- (HEIGHT - 1) until (HEIGHT - 4) by -1
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j - 1) && actual == table(i + 2)(j - 2) && actual == table(i + 3)(j - 3)) {
          if (endCheck)
            println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + (j - 1) + ", " + (i + 2) + "x" + (j - 2) + ", " + (i + 3) + "x" + (j - 3) + ", ")
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

  def checkDiagonallyDownRight(table: Array[Array[Int]], color: Int, endCheck: Boolean): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- 0 until (HEIGHT - 3)
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j + 1) && actual == table(i + 2)(j + 2) && actual == table(i + 3)(j + 3)) {
          if (endCheck)
            println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + (j + 1) + ", " + (i + 2) + "x" + (j + 2) + ", " + (i + 3) + "x" + (j + 3) + ", ")
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

  def checkWin(table: Array[Array[Int]], endCheck: Boolean = false): Int = {
    if (checkVertically(table, RED, endCheck) || checkHorizontally(table, RED, endCheck) || checkDiagonallyDownRight(table, RED, endCheck) || checkDiagonallyUpRight(table, RED, endCheck))
      RED
    else if (checkVertically(table, YELLOW, endCheck) || checkHorizontally(table, YELLOW, endCheck) || checkDiagonallyDownRight(table, YELLOW, endCheck) || checkDiagonallyUpRight(table, YELLOW, endCheck))
      YELLOW
    else
      NEUTRAL
  }

  def gravity(table: Array[Array[Int]], column: Int): Int = {
    table(column).map {
      case NEUTRAL => 1
      case _ => 0
    }.sum - 1
  }

  def makeMove(): Unit = {
    val column = Gui.getInputColumn
    val row = gravity(board, column)
    if (row < 0)
      makeMove()
    else
      board(column)(row) = RED
  }

  def AImakeMove(ai: AI): Unit = {
    val column = ai.makeMove()
    val row = gravity(board, column)
    if (row < 0)
      AImakeMove(ai)
    else {
      board(column)(row) = YELLOW
    }
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
  }
}


