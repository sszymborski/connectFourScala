package connectFour

import connectFour.Game.{HEIGHT, WIDTH}

object Gui {

  def display(): Unit = {
    for (_ <- 1 to 5) {
      for (_ <- 1 to 25)
        print("*")
      println(" ")
    }

    val printBoard = Game.board.map(_.map {
      case Game.RED => "R"
      case Game.YELLOW => "Y"
      case Game.NEUTRAL => "-"
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

  def getInput: Int = {
    println("Which column you choose? [0-6]")
    val in = scala.io.StdIn.readInt()
    if (in < 0 || in > 6)
      getInput
    else
      in
  }

}
