import Direction.*
import Colour.*

enum Colour:
  case White, Black
  def sameColour(c: Colour): Boolean =
    if (this == White && c == White) || (this == Black && c == Black) then true
    else false

abstract class Piece(val colour: Colour, val square: Square):

  def move(to: Square): Piece =
    this match
      case _ if this.representation == "R" => Rook(this.colour, to)
      case _ if this.representation == "Q" => Queen(this.colour, to)
      case _ if this.representation == "L" => Loper(this.colour, to)
      case _ if this.representation == "B" => Bishop(this.colour, to)
      case _ if this.representation == "K" => King(this.colour, to)
  def legalMoves(cb: ChessBoard): List[Square]
  def representation: String = ""
  def value = 1 // default piece value

  override def toString: String =
    if colour == White then representation.toUpperCase
    else representation.toLowerCase

class Loper(colour: Colour, square: Square) extends Piece(colour, square):

  override def legalMoves(cb: ChessBoard): List[Square] =
    // getOrElse Squares  that result in None returns squares that will result in invalid adjacent
    val middleSquare = square.adjacent(North, cb).getOrElse(Square('a', 8)).adjacent(North, cb)
    val leftOfMiddle = middleSquare.getOrElse(Square('a', 1)).adjacent(East, cb)
    val rightOfMiddle = middleSquare.getOrElse(Square('h', 8)).adjacent(West,cb)
     middleSquare match
       case _: Some[Square] if leftOfMiddle.isEmpty && rightOfMiddle.isDefined => List[Square](rightOfMiddle.get)
       case _: Some[Square] if leftOfMiddle.isDefined && rightOfMiddle.isEmpty => List[Square](leftOfMiddle.get)
       case _: Some[Square] if leftOfMiddle.isDefined && rightOfMiddle.isDefined => List[Square](leftOfMiddle.get, rightOfMiddle.get)
       case _ => List()

  override def value = Loper.value

  override def representation = Loper.representation

object Loper:
  private val value = 2
  private val representation = "L"

abstract class LinearPiece(colour: Colour, square: Square) extends Piece(colour, square):
  def directions: List[Direction]

  def legalMoves(chessBoard: ChessBoard): List[Square] =
    this match
      case _ if this.representation == "K" => directions.flatMap(direction => List(legalMovesInDirection(direction, chessBoard)
        .headOption
        .getOrElse(Square('?',-1)))
        .filter(_ != Square('?', -1)))
      case _ => directions.flatMap(direction => legalMovesInDirection(direction, chessBoard))

  private def legalMovesInDirection(direction: Direction,cb: ChessBoard): List[Square] =

    def nextSquares(square: Square): List[Square] =
      square.adjacent(direction, cb) match
        case sq: Some[Square]  if cb.isEmpty(sq.get)  =>
          sq.get :: nextSquares(sq.get)
        case sq: Some[Square] if (cb.isOccupied(sq.get) && pieceOnSquareHasOppositeColour(sq.get, cb)) =>
          sq.get :: List()
        case _ => List()
    nextSquares(square)

  private def pieceOnSquareHasOppositeColour(s: Square, chessBoard: ChessBoard): Boolean =
    chessBoard.pieceOnSquare(s) match
      case Some(p) => !p.colour.sameColour(colour)
      case None => false

class Rook(colour: Colour, square: Square) extends LinearPiece(colour, square):
  def directions: List[Direction] = List(North, South, East, West)
  override def value = Rook.value
  override def representation = Rook.representation

object Rook:
  private val value = 5
  private val representation = "R"


class King(colour: Colour, square: Square) extends LinearPiece(colour, square):
  def directions: List[Direction] = List(North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest)
  override def value = King.value

  override def representation = King.representation

object King:
  private val value = 0
  private val representation = "K"

class Bishop(colour: Colour, square: Square) extends LinearPiece(colour, square):
  def directions: List[Direction] = List(NorthEast, NorthWest, SouthEast, SouthWest)
  override def representation = Bishop.representation
  override def value = Bishop.value

object Bishop:
  val value = 3
  private val representation = "B"

class Queen(colour: Colour, square: Square) extends LinearPiece(colour, square):
  def directions: List[Direction] = List(North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest)
  override def representation = Queen.representation
  override def value = Queen.value

object Queen:
  val value = 9
  private val representation = "Q"