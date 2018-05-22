package connectFour

import scala.util.control.Breaks._

object Game extends App {

  println("Hello from Scala!")

  val RED = 1
  val YELLOW = 2
  val WIDTH = 7
  val HEIGHT = 6
  val NEUTRAL = 0

  val board = Array.ofDim[Int](WIDTH, HEIGHT)
  val whoPlays = true

  def checkVertically(color: Int): Boolean = {
    val result = for {i <- 0 until WIDTH
                      j <- (HEIGHT - 3) until HEIGHT
                      if board(i)(j) != NEUTRAL} yield {
      if (board(i)(j) == color) {
        val actual = board(i)(j)
        if ((actual == board(i)(j - 1)) && (actual == board(i)(j - 2)) && (actual == board(i)(j - 3))) {
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

  def checkHorizontally(color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until HEIGHT
                      if board(i)(j) != NEUTRAL} yield {
      if (board(i)(j) == color) {
        val actual = board(i)(j)
        if (actual == board(i + 1)(j) && actual == board(i + 2)(j) && actual == board(i + 3)(j)) {
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

  def checkDiagonallyUpRight(color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- (HEIGHT - 3) until HEIGHT
                      if board(i)(j) != NEUTRAL} yield {
      if (board(i)(j) == color) {
        val actual = board(i)(j)
        if (actual == board(i + 1)(j - 1) && actual == board(i + 2)(j - 2) && actual == board(i + 3)(j - 3)) {
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

  def checkDiagonallyDownRight(color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until (HEIGHT - 3)
                      if board(i)(j) != NEUTRAL} yield {
      if (board(i)(j) == color) {
        val actual = board(i)(j)
        if (actual == board(i + 1)(j + 1) && actual == board(i + 2)(j + 2) && actual == board(i + 3)(j + 3)) {
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

  def checkWin(): Int = {
    if (checkVertically(RED) || checkHorizontally(RED) || checkDiagonallyDownRight(RED) || checkDiagonallyUpRight(RED))
      RED
    else if (checkVertically(YELLOW) || checkHorizontally(YELLOW) || checkDiagonallyDownRight(YELLOW) || checkDiagonallyUpRight(YELLOW))
      YELLOW
    else
      NEUTRAL
  }

  def gravity(column: Int): Int = {
    board(column).map {
      case NEUTRAL => 1
      case _ => 0
    }.sum - 1
  }

  def makeMove(): Unit = {
    val column = Gui.getInput()
    val row = gravity(column)
    if (row < 0)
      makeMove()
    else
      board(column)(row) = RED
  }

  def AImakeMove(): Unit = {
    val column = AI.makeRandomMove()
    val row = gravity(column)
    if (row < 0)
      AImakeMove()
    else
      board(column)(row) = YELLOW
  }

  checkWin()

  Gui.display()

  breakable {
    for (i <- 1 to 21) {
      makeMove()
      Gui.display()
      if (checkWin() != NEUTRAL) break
      AImakeMove()
      Gui.display()
      if (checkWin() != NEUTRAL) break
    }
  }


}


