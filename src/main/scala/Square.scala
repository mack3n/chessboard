
// assume moving North increases row index and moving South increases column index
enum Direction:
  case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest

case class Square(column: Char, row: Int):
  // Return adjacent square in given direction
  def adjacent(direction: Direction, chessBoard: ChessBoard): Option[Square] =
    val columnIndex = Conversions.charToIndex(column)
    direction match
      case Direction.North if row < chessBoard.dimension => Some(Square(column, row + 1))
      case Direction.South if row > 1 => Some(Square(column, row - 1))
      case Direction.East if columnIndex < chessBoard.dimension => Some(Square(Conversions.indexToChar(columnIndex + 1), row))
      case Direction.West if columnIndex > 1 => Some(Square(Conversions.indexToChar(columnIndex - 1), row))
      case Direction.NorthEast if row < chessBoard.dimension && columnIndex < chessBoard.dimension => Some(Square(Conversions.indexToChar(columnIndex + 1), row + 1))
      case Direction.NorthWest if row < chessBoard.dimension && columnIndex > 1 => Some(Square(Conversions.indexToChar(columnIndex - 1), row + 1))
      case Direction.SouthEast if row > 1 && columnIndex < chessBoard.dimension => Some(Square(Conversions.indexToChar(columnIndex + 1), row - 1))
      case Direction.SouthWest if row > 1 && columnIndex > 1 => Some(Square(Conversions.indexToChar(columnIndex - 1), row - 1))
      case _ => None
  
  override def toString =
    s"$column$row"