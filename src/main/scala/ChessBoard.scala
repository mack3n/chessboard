import Conversions.*
import Colour.*

class ChessBoard(val dimension: Int = 8, val pieces: List[Piece] = List()):
  def addPiece(piece: Piece) =
    if pieces.exists(_.square == piece.square) then this
    else ChessBoard(dimension, piece :: pieces)

  def noOfPieces = pieces.length
  
  def pieceOnSquare(square: Square): Option[Piece] =
    pieces.find(_.square == square) 

  def isEmpty(square: Square) = pieceOnSquare(square) == None

  def isOccupied(square: Square) = pieceOnSquare(square) != None

  def assessment: Int =
    val totals = pieces.foldLeft(0, 0)(
      (totalsSoFar, piece) =>
        if piece.colour == White then
          (totalsSoFar(0) + piece.value, totalsSoFar(1))
        else
          (totalsSoFar(0), totalsSoFar(1) + piece.value)
    )
    totals(0) - totals(1)

  private def convertRowToString(row: Int) =
    (1 to dimension).toList.map(
      i => "  " + (pieceOnSquare(Square(Conversions.indexToChar(i), row)) match
        case Some(p) => p.toString
        case None => "*"
      )
    ).mkString

  def applyMove(initial: Square, dest: Square): ChessBoard =
    val piece = pieceOnSquare(initial)
    piece match
      case piece: Some[Piece] =>
        if piece.get.legalMoves(this).contains(dest) then
          // returns chessboard by adding the piece to the destination and then filtering out the piece on its current square
          ChessBoard(dimension, piece.get.move(dest) :: pieces.filter(_ == piece))
        else
          this
      case _ => this

  override def toString =
    (dimension to 1 by -1).toList.map(
      i => s"$i" + convertRowToString(i)
    ).mkString("", "\n", "") +
    "\n   " + (1 to dimension).map(indexToChar).mkString("  ")

  def toStringAlt: String =
    pieces.map(p => p.toString + p.square.toString).sorted.mkString(", ")


