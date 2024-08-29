import Colour.*
import org.scalatest.funsuite.AnyFunSuite

class QueenSuite extends AnyFunSuite {
  test("Queen: White Queen constructed correctly") {
    val queen = Queen(White, Square('h', 7))
    assert(queen.toString === "Q")
    assert(queen.square === Square('h', 7))
  }

  test("Queen: Black Queen constructed correctly") {
    val queen = Queen(Black, Square('a', 1))
    assert(queen.toString === "q")
    assert(queen.square === Square('a', 1))
  }



  test("Queen: legalMoves works for lone queen midboard") {
    val queen = Queen(Black, Square('d', 4))
    val cb = ChessBoard()
      .addPiece(queen)
    val moves = queen.legalMoves(cb)
    assert(moves.length === 27)
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('f', 6)))
    assert(moves.contains(Square('g', 7)))
    assert(moves.contains(Square('h', 8)))

    assert(moves.contains(Square('c', 3)))
    assert(moves.contains(Square('b', 2)))
    assert(moves.contains(Square('a', 1)))

    assert(moves.contains(Square('c', 5)))
    assert(moves.contains(Square('b', 6)))
    assert(moves.contains(Square('a', 7)))
    
    assert(moves.contains(Square('e', 3)))
    assert(moves.contains(Square('f', 2)))
    assert(moves.contains(Square('g', 1)))
    
    for i <- 5 to cb.dimension do
      assert(moves.contains(Square('d', i)))
    for i <- 1 to 3 do
      assert(moves.contains(Square('d', i)))
    for c <- 'a' to 'c' do
      assert(moves.contains(Square(c, 4)))
    for c <- 'e' to 'h' do
      assert(moves.contains(Square(c, 4)))
  }


  test("Queen: legalMoves works with multiple pieces various colour") {
    val queen = Queen(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Rook(White, Square('g', 6)))
      .addPiece(Queen(Black, Square('b', 7)))
      .addPiece(Queen(White, Square('h', 1)))
      .addPiece(Rook(Black, Square('c', 2)))
    val moves = queen.legalMoves(cb)

    assert(moves.length === 8)
    assert(moves.contains(Square('f', 5)))

    assert(moves.contains(Square('d', 5)))
    assert(moves.contains(Square('c', 6)))
    assert(moves.contains(Square('b', 7)))

    assert(moves.contains(Square('f', 3)))
    assert(moves.contains(Square('g', 2)))

    assert(moves.contains(Square('d', 3)))
    assert(moves.contains(Square('c', 2)))
  }


  test("Queen: legalMoves works for lone queen in corner 1") {
    val queen = Queen(Black, Square('a', 1))
    val cb = ChessBoard()
      .addPiece(queen)
    val moves = queen.legalMoves(cb)
    assert(moves.length === 7)
    val rows = List.range(2, cb.dimension)
    val cols = List.range('b', 'h')
    cols.zip(rows).foreach(
      (r, c) => assert(moves.contains(Square(r, c)))
    )
  }


  test("Queen: legalMoves works for lone queen in corner 2") {
    val queen = Queen(White, Square('a', 8))
    val cb = ChessBoard()
      .addPiece(queen)
    val moves = queen.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 1 to cb.dimension-1 do
      assert(moves.contains(Square('a', i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, cb.dimension)))
  }

  test("Queen: legalMoves works for lone queen in corner 3") {
    val queen = Queen(White, Square('h', 8))
    val cb = ChessBoard()
      .addPiece(queen)
    val moves = queen.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 1 to cb.dimension - 1 do
      assert(moves.contains(Square('h', i)))
    for c <- 'a' to 'g' do
      assert(moves.contains(Square(c, cb.dimension)))
  }

  test("Queen: legalMoves works for lone queen in corner 4") {
    val queen = Queen(White, Square('h', 1))
    val cb = ChessBoard()
      .addPiece(queen)
    val moves = queen.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 2 to cb.dimension do
      assert(moves.contains(Square('h', i)))
    for c <- 'a' to 'g' do
      assert(moves.contains(Square(c, 1)))
  }

  test("Queen: legalMoves works with multiple queens same colour") {
    val queen = Queen(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('e', 6)))
      .addPiece(Queen(White, Square('e', 3)))
      .addPiece(Queen(White, Square('a', 4)))
      .addPiece(Queen(White, Square('g', 4)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 5)
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('b', 4)))
    assert(moves.contains(Square('c', 4)))
    assert(moves.contains(Square('d', 4)))
    assert(moves.contains(Square('f', 4)))
  }

  test("Queen: legalMoves works with single queen same colour 1") {
    val COL = 'e'
    val ROW = 4
    val queen = Queen(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('e', 6)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 11)
    assert(moves.contains(Square(COL, 5)))
    for i <- 1 to 3 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'a' to 'd' do
      assert(moves.contains(Square(c, ROW)))
    for c <- 'f' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }

  test("Queen: legalMoves works with single queen same colour 2") {
    val COL = 'a'
    val ROW = 1
    val queen = Queen(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('a', 8)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 13)
    for i <- 2 to 7 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }


  test("Queen: legalMoves works with single queen different colour 1") {
    val COL = 'e'
    val ROW = 4
    val queen = Queen(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(Black, Square('e', 6)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 12)
    assert(moves.contains(Square(COL, 5)))
    assert(moves.contains(Square(COL, 6)))
    for i <- 1 to 3 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'a' to 'd' do
      assert(moves.contains(Square(c, ROW)))
    for c <- 'f' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }

  test("Queen: legalMoves works with single queen different colour 2") {
    val COL = 'a'
    val ROW = 1
    val queen = Queen(Black, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('a', 8)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 2 to 8 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }

  test("Queen: legalMoves works with multiple queens opposite colour") {
    val queen = Queen(Black, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('e', 6)))
      .addPiece(Queen(White, Square('e', 3)))
      .addPiece(Queen(White, Square('a', 4)))
      .addPiece(Queen(White, Square('g', 4)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 9)
    assert(moves.contains(Square('e', 6)))
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('e', 3)))
    assert(moves.contains(Square('a', 4)))
    assert(moves.contains(Square('b', 4)))
    assert(moves.contains(Square('c', 4)))
    assert(moves.contains(Square('d', 4)))
    assert(moves.contains(Square('f', 4)))
    assert(moves.contains(Square('g', 4)))
  }

  test("Queen: legalMoves works with multiple queens various colour") {
    val queen = Queen(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(queen)
      .addPiece(Queen(White, Square('e', 6)))
      .addPiece(Queen(Black, Square('e', 3)))
      .addPiece(Queen(White, Square('a', 4)))
      .addPiece(Queen(Black, Square('g', 4)))
    val moves = queen.legalMoves(cb)
    assert(moves.length === 7)
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('e', 3)))
    assert(moves.contains(Square('b', 4)))
    assert(moves.contains(Square('c', 4)))
    assert(moves.contains(Square('d', 4)))
    assert(moves.contains(Square('f', 4)))
    assert(moves.contains(Square('g', 4)))
  }
}
