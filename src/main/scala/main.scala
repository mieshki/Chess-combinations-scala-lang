package ChessChallenge

import ChessChallenge.ChessBoard._


object main extends App {

  def findAllCombinationsWithoutBeats(chessBoard: ChessBoard, piecesList: List[ChessPiece]){

    println("Press Enter to start finding all possible combinations")
    scala.io.StdIn.readLine()
    println("Board size: " + chessBoard.width  + "x" + chessBoard.height)
    println("Chess pieces: " + piecesList)
    println("")
    println("Program is running, it may take a while")
    println("")

    val t1 = System.nanoTime

    findAllCombinations(chessBoard, piecesList)

    val t2 = System.nanoTime()

    goodBoards = goodBoards.distinct // to delete duplicates

    val howManyResultsToDisplay = 5

    val goodBoardsToPrint = goodBoards.take(howManyResultsToDisplay)

    for(i <- 0 until goodBoardsToPrint.length){
      println((i + 1) + ": " + goodBoards(i))
    }

    println("")

    println("Total combinations without possible beats: " + goodBoards.length)

    val duration = (t2 - t1) / 1e9d

    println("Elapsed time: " + duration)
  }// end def findAllCombinationsWithoutBeats


  val m = 6 // width
  val n = 5 // height

  val chessBoardToCheck = new ChessBoard(m, n)
  val chessPiecesToCheck = List(King, King, Queen, Queen, Bishop, Bishop, Knight)

  findAllCombinationsWithoutBeats(chessBoardToCheck, chessPiecesToCheck)

}
