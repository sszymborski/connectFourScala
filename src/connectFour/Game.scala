package connectFour

import scala.util.control.Breaks._

object Game extends App {

  println("Hello from Scala!")

  val RED: Int = 1
  val YELLOW: Int = 2
  val WIDTH: Int = 7
  val HEIGHT: Int = 6
  val NEUTRAL: Int = 0

  val board = Array.ofDim[Int](WIDTH, HEIGHT)
  val whoPlays = true

  def checkVertically(table: Array[Array[Int]], color: Int): Boolean = {
    val result = for {i <- 0 until WIDTH
                      j <- (HEIGHT - 3) until HEIGHT
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if ((actual == table(i)(j - 1)) && (actual == table(i)(j - 2)) && (actual == table(i)(j - 3))) {
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

  def checkHorizontally(table: Array[Array[Int]], color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until HEIGHT
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j) && actual == table(i + 2)(j) && actual == table(i + 3)(j)) {
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

  def checkDiagonallyUpRight(table: Array[Array[Int]], color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- (HEIGHT - 3) until HEIGHT
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j - 1) && actual == table(i + 2)(j - 2) && actual == table(i + 3)(j - 3)) {
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

  def checkDiagonallyDownRight(table: Array[Array[Int]], color: Int): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until (HEIGHT - 3)
                      if table(i)(j) != NEUTRAL} yield {
      if (table(i)(j) == color) {
        val actual = table(i)(j)
        if (actual == table(i + 1)(j + 1) && actual == table(i + 2)(j + 2) && actual == table(i + 3)(j + 3)) {
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

  def checkWin(table: Array[Array[Int]]): Int = {
    if (checkVertically(table, RED) || checkHorizontally(table, RED) || checkDiagonallyDownRight(table, RED) || checkDiagonallyUpRight(table, RED))
      RED
    else if (checkVertically(table, YELLOW) || checkHorizontally(table, YELLOW) || checkDiagonallyDownRight(table, YELLOW) || checkDiagonallyUpRight(table, YELLOW))
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
    val column = Gui.getInput()
    val row = gravity(board, column)
    if (row < 0)
      makeMove()
    else
      board(column)(row) = RED
  }

  def AImakeMove(): Unit = {
    val column = AI.makeMove()
    val row = gravity(board, column)
    if (row < 0)
      AImakeMove()
    else {
      board(column)(row) = YELLOW
      println(row + " " + column)
    }
  }

  checkWin(board)

  Gui.display()

  breakable {
    for (i <- 1 to 21) {
      makeMove()
      Gui.display()
      if (checkWin(board) != NEUTRAL) break
      AImakeMove()
      Gui.display()
      if (checkWin(board) != NEUTRAL) break
    }
  }


}


