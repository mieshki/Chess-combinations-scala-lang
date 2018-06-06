package ChessChallenge

import scala.annotation.tailrec

import main._

object ChessBoard {

  case class ChessBoard(var width: Int, var height: Int){
    var allChessPiecesOnBoard: Map[Cell, ChessPiece] = Map()
  }// end case class ChessBoard

  var allCellsOnBoard: List[Cell] = List()

  for(h <- 0 until n){
    for(w <- 0 until m){
      allCellsOnBoard +:= Cell(w, h)
    }
  }

  case class Cell(x: Int, y: Int)

  // function that map possible pieces with cells and validate if any chess piece can beat another
  def checkBoard(chessBoardToCheck: ChessBoard): Boolean = {
    val allChessPiecesOnBoardToCheck = chessBoardToCheck.allChessPiecesOnBoard
    var chessPiecesToLoop = allChessPiecesOnBoardToCheck

    // loop function that check every chess piece on board can beat something
    @tailrec
    def loop(currentChecking: (Cell, ChessPiece), allPieces: Map[Cell, ChessPiece], counter: Int): Boolean = {
      if(allPieces.isEmpty){ // possible beats checked for current chess piece
        if(counter != allChessPiecesOnBoardToCheck.size){
          chessPiecesToLoop = chessPiecesToLoop.tail  // current chess piece possible beats checked, remove checked chess piece
          loop(chessPiecesToLoop.head, allChessPiecesOnBoardToCheck, counter + 1) // set another chess piece to check possible beats
        }
        else true //all possible beats checked for every chess piece on board if reached end it means there's no possible beats for all chess pieces
      }
      else if(currentChecking._2(currentChecking._1, allPieces.head._1) && currentChecking != allPieces.head) false //current chess piece has possible beat
      else loop(currentChecking, allPieces.tail, counter) // current chess piece don't have possible beat
    }// end def loop

    // if there are any chess pieces start checking
    if(allChessPiecesOnBoardToCheck.nonEmpty) loop(allChessPiecesOnBoardToCheck.head, allChessPiecesOnBoardToCheck, 1)// starts looping
    else true // if not return true what means board don't have possible beats because it don't have any chess pieces

    //return true - if there's no possible beats on board or false - when there is any possible beat
  }// end def checkBoard


  // variable outside of function because it's used in findAllCombinationsWithoutBeats function to print results
  var goodBoards: List[Map[Cell, ChessPiece]] = List()
  // function that find all possible combinations with list of chess pieces and check if there's any combination without possible beat, if so addition to goodBoards list
  def findAllCombinations(chessBoard: ChessBoard, chessPieces: List[ChessPiece]){
    @tailrec
    def loopThroughEmptyCells(allCells: List[Cell], chessPiece: ChessPiece, patternChessBoard: ChessBoard): Unit = {
      if(allCells.isEmpty) {} // loop with current pattern chess board is done
      else if(!chessBoard.allChessPiecesOnBoard.exists(_._1 == allCells.head)){ // found empty cell

        val tempPatternChessBoard = patternChessBoard.copy()
        tempPatternChessBoard.allChessPiecesOnBoard = patternChessBoard.allChessPiecesOnBoard
        tempPatternChessBoard.allChessPiecesOnBoard += allCells.head -> chessPiece // insert chess piece

        if(checkBoard(tempPatternChessBoard)){ // if board already don't have possible beats
          // not every chess piece is placed on board
          if(chessPieces.length == 1) goodBoards +:= tempPatternChessBoard.allChessPiecesOnBoard // +: addition at the beginning because it's such faster than addition and the (:+) end
          else if(chessPieces.length > 1) findAllCombinations(tempPatternChessBoard, chessPieces.tail) // It means every chess piece is placed on board added to avoid addition boards without all chess pieces and still don't have any possible beats
        }

        loopThroughEmptyCells(allCells.tail, chessPiece, patternChessBoard) // recursive call
      }
      else { // found filled cell
        loopThroughEmptyCells(allCells.tail, chessPiece, patternChessBoard) // skip cell
      }
    }// end def loopThroughEmptyCells

    loopThroughEmptyCells(allCellsOnBoard, chessPieces.head, chessBoard)
  }// end def findAllCombinations

}// end object ChessBoard

