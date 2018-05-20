package connectFour

object Game extends App {

  println("Hello from Scala!")

  //TODO główna pętla gry, plansza (board)
  //TODO przyjmowanie ruchów od gracza (gui.getMove(board))
  //TODO prośba o ruch do AI (AI.makeMove(board))


  val board = Array.ofDim[Int](7, 6)
  val whoPlays = true

  def checkWin(): Boolean = {

    for {i <- 1 to board.length
         j <- 1 to board(0).length
         if board(i)(j) != 0} yield
      print(board(i)(j))
    false
  }

}
