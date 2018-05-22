package connectFour

object Game extends App {

  println("Hello from Scala!")

  //TODO główna pętla gry, plansza (board)
  //TODO przyjmowanie ruchów od gracza (gui.getMove(board))
  //TODO prośba o ruch do AI (AI.makeMove(board))

  val RED = 1
  val YELLOW = 1
  val WIDTH = 7
  val HEIGHT = 6

  val board = Array.ofDim[Int](WIDTH, HEIGHT)
  val whoPlays = true

  def checkVertically(): Boolean = {
    val result = for {i <- 0 until WIDTH
                      j <- (HEIGHT - 3) until HEIGHT
                      if board(i)(j) != 0} yield {
      val actual = board(i)(j)
      if ((actual == board(i)(j - 1)) && (actual == board(i)(j - 2)) && (actual == board(i)(j - 3))) {
        println("Win by " + i + "x" + j + ", " + i + "x" + (j - 1) + ", " + i + "x" + (j - 2) + ", " + i + "x" + (j - 3) + ", ")
        1
      }
      else
        0
    }
    result.sum > 0
  }

  def checkHorizontally(): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until HEIGHT
                      if board(i)(j) != 0} yield {
      val actual = board(i)(j)
      if (actual == board(i + 1)(j) && actual == board(i + 2)(j) && actual == board(i + 3)(j)) {
        println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + j + ", " + (i + 2) + "x" + j + ", " + (i + 3) + "x" + j + ", ")
        1
      }
      else
        0
    }
    result.sum > 0
  }

  def checkDiagonallyUpRight(): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- (HEIGHT - 3) until HEIGHT
                      if board(i)(j) != 0} yield {
      val actual = board(i)(j)
      if (actual == board(i + 1)(j - 1) && actual == board(i + 2)(j - 2) && actual == board(i + 3)(j - 3)) {
        println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + (j - 1) + ", " + (i + 2) + "x" + (j - 2) + ", " + (i + 3) + "x" + (j - 3) + ", ")
        1
      }
      else
        0
    }
    result.sum > 0
  }

  def checkDiagonallyDownRight(): Boolean = {
    val result = for {i <- 0 until (WIDTH - 3)
                      j <- HEIGHT until (HEIGHT - 3)
                      if board(i)(j) != 0} yield {
      val actual = board(i)(j)
      if (actual == board(i + 1)(j + 1) && actual == board(i + 2)(j + 2) && actual == board(i + 3)(j + 3)) {
        println("Win by " + i + "x" + j + ", " + (i + 1) + "x" + (j + 1) + ", " + (i + 2) + "x" + (j + 2) + ", " + (i + 3) + "x" + (j + 3) + ", ")
        1
      }
      else
        0
    }
    result.sum > 0
  }

  def checkWin(): Boolean = {
    checkVertically() || checkHorizontally() || checkDiagonallyDownRight() || checkDiagonallyUpRight()
  }

  checkWin()

  Gui.display()
  Gui.getInput()
  Gui.display()

}
