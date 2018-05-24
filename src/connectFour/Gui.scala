package connectFour

import connectFour.Game._

/**
  * Object responsible for getting input and displaying output.
  *
  */
object Gui {

  /**
    * Function printing current state of game board in console.
    *
    */
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

  /**
    * Function displaying message about win.
    *
    * @param a1 column number of first puck
    * @param a2 row number of first puck
    * @param b1 column number of second puck
    * @param b2 row number of second puck
    * @param c1 column number of third puck
    * @param c2 row number of third puck
    * @param d1 column number of fourth puck
    * @param d2 row number of fourth puck
    */
  def showWinMessage(a1: Int, a2: Int, b1: Int, b2: Int, c1: Int, c2: Int, d1: Int, d2: Int): Unit = {
    println("Win by " + a1 + "x" + a2 + ", " + b1 + "x" + b2 + ", " + c1 + "x" + c2 + ", " + d1 + "x" + d2 + ", ")
  }

  /**
    * Function displaying message about draw.
    *
    */
  def showDrawMessage(): Unit = {
    println("Draw")
  }

  /**
    * Function getting from player a number of the column for his next move.
    *
    * @return number of column for the next player's move
    */
  def getInputColumn: Int = {
    println("Which column do you choose? [0-6]")
    val in = scala.io.StdIn.readInt()
    if (in < 0 || in > 6)
      getInputColumn
    else
      in
  }

  /**
    * Function getting from player a depth of AI's tree.
    *
    * @return depth of AI's tree
    */
  def getInputDepth: Int = {
    println("Choose depth of AI's search tree: ")
    val in = scala.io.StdIn.readInt()
    if (in < 1)
      getInputDepth
    else
      in
  }
}
