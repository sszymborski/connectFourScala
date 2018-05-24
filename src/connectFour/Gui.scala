package connectFour

import connectFour.Game._

object Gui {

  def display(): Unit = {
    for (_ <- 1 to 5) {
      for (_ <- 1 to 25)
        print("*")
      println(" ")
    }

    val printBoard = board.map(_.map {
      case RED => "R"
      case YELLOW => "Y"
      case NEUTRAL => "-"
    })

    for (j <- 0 until HEIGHT) {
      for (i <- 0 until WIDTH)
        print(printBoard(i)(j) + "\t")
      println(" ")
    }

    for (_ <- 1 to 5) {
      for (_ <- 1 to 25)
        print("*")
      println(" ")
    }
  }

  def showWinMessage(a1: Int, a2: Int, b1: Int, b2: Int, c1: Int, c2: Int, d1: Int, d2: Int): Unit = {
    println("Win by " + a1 + "x" + a2 + ", " + b1 + "x" + b2 + ", " + c1 + "x" + c2 + ", " + d1 + "x" + d2 + ", ")
  }

  def showDrawMessage(): Unit = {
    println("Draw")
  }

  def getInputColumn: Int = {
    println("Which column do you choose? [0-6]")
    val in = scala.io.StdIn.readInt()
    if (in < 0 || in > 6)
      getInputColumn
    else
      in
  }

  def getInputDepth: Int = {
    println("Choose depth of AI's search tree: ")
    val in = scala.io.StdIn.readInt()
    if (in < 1)
      getInputDepth
    else
      in
  }
}
