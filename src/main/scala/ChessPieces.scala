package ChessChallenge

import ChessBoard._
import math.abs


abstract class ChessPiece{
  val charSymbol: Char
  def apply(currentCell: Cell, destinationCell: Cell): Boolean

  override def toString: String = charSymbol.toString // used to print chess pieces symbols at the end, without it show hashcode
}

object King extends ChessPiece{
  val charSymbol = 'K'
  def apply(currentCell: Cell, destinationCell: Cell): Boolean = {
    // King moves in every direction in max range 1 cell
    abs(destinationCell.x - currentCell.x) <= 1 && abs(destinationCell.y - currentCell.y) <= 1
  }
}

object Queen extends ChessPiece{
  val charSymbol = 'Q'
  def apply(currentCell: Cell, destinationCell: Cell): Boolean = {
    // Queen moves in every direction in infinite range
    currentCell.x == destinationCell.x || currentCell.y == destinationCell.y ||
      abs(destinationCell.x - currentCell.x) == abs(destinationCell.y - currentCell.y)
  }
}

object Bishop extends ChessPiece{
  val charSymbol = 'B'
  def apply(currentCell: Cell, destinationCell: Cell): Boolean = {
    // Bishop moves diagonal in infinite range
    abs(destinationCell.x - currentCell.x) == abs(destinationCell.y - currentCell.y)
  }
}

object Rook extends ChessPiece{
  val charSymbol = 'R'
  def apply(currentCell: Cell, destinationCell: Cell): Boolean = {
    // Rook moves horizontally or vertically in infinite range
    currentCell.x == destinationCell.x || currentCell.y == destinationCell.y
  }
}

object Knight extends ChessPiece{
  val charSymbol = 'N'
  def apply(currentCell: Cell, destinationCell: Cell): Boolean = {
    // Knight moves like 'L' letter
    (abs(destinationCell.x - currentCell.x) == 2 && abs(destinationCell.y - currentCell.y) == 1) ||
      (abs(destinationCell.x - currentCell.x) == 1 && abs(destinationCell.y - currentCell.y) == 2)
  }
}
