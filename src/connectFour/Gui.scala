package connectFour

object Gui {

  //TODO wyświetlanie kounikatów na konsolę
  //TODO wyświetlanie planszy na konsolę
  //TODO wczytywanie ruchu od gracza


  def display() : Unit = {
    for (j <- 0 until Game.HEIGHT) {
      for (i <- 0 until Game.WIDTH) {
        print(Game.board(i)(j) + "\t")
      }
      println(" ")
    }
  }

  def getInput() : Int = {
    println("Which column you choose? [0-6]")
    Console.readInt()
  }

}
