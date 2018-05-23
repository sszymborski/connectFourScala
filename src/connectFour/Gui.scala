package connectFour

object Gui {

  def display(): Unit = {
    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
    }

    val printBoard = Game.board.map(_.map {
      case Game.RED => "R"
      case Game.YELLOW => "Y"
      case Game.NEUTRAL => "0"
    })

    for (j <- 0 until Game.HEIGHT) {
      for (i <- 0 until Game.WIDTH)
        print(printBoard(i)(j) + "\t")
      println(" ")
    }

    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
    }
  }

  def getInput(): Int = {
    println("Which column you choose? [0-6]")
    val in = Console.readInt()
    if (in < 0 || in > 6)
      getInput()
    else
      in
  }

}
