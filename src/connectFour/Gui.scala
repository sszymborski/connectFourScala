package connectFour

object Gui {

  def display(): Unit = {
    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
    }
    for (j <- 0 until Game.HEIGHT) {
      for (i <- 0 until Game.WIDTH) {
        if (Game.board(i)(j) == 1)
          print("R" + "\t")
        else if (Game.board(i)(j) == 2)
          print("Y" + "\t")
        else
          print(Game.board(i)(j) + "\t")
      }
      println(" ")
    }
    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
    }
  }

  def display2(board: Array[Array[Int]]): Unit = {
//    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
//    }
    for (j <- 0 until Game.HEIGHT) {
      for (i <- 0 until Game.WIDTH) {
        if (board(i)(j) == 1)
          print("R" + "\t")
        else if (board(i)(j) == 2)
          print("Y" + "\t")
        else
          print(board(i)(j) + "\t")
      }
      println(" ")
    }
//    for (i <- 1 to 5) {
      for (j <- 1 to 25)
        print("*")
      println(" ")
//    }
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
