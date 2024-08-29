import Colour.*
import org.scalatest.funsuite.AnyFunSuite

class BishopSuite extends AnyFunSuite {
  test("Bishop: White Bishop constructed correctly") {
    val bishop = Bishop(White, Square('h', 7))
    assert(bishop.toString === "B")
    assert(bishop.square === Square('h', 7))
  }

  test("MelPart4.2 Test if Bishop value ok") {
    val bishop = Bishop(White, Square('h', 7))
    assert(bishop.value === 3)
  }

  test("Bishop: Black Bishop constructed correctly") {
    val bishop = Bishop(Black, Square('a', 1))
    assert(bishop.toString === "b")
    assert(bishop.square === Square('a', 1))
  }

  test("Bishop: legalMoves works for lone bishop midboard") {
    val bishop = Bishop(Black, Square('d', 4))
    val cb = ChessBoard()
      .addPiece(bishop)
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 13)
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
  }

  test("Bishop: legalMoves works with multiple pieces various colour") {
    val bishop = Bishop(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Rook(White, Square('g', 6)))
      .addPiece(Bishop(Black, Square('b', 7)))
      .addPiece(Bishop(White, Square('h', 1)))
      .addPiece(Rook(Black, Square('c', 2)))
    val moves = bishop.legalMoves(cb)

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


  test("Bishop: legalMoves works for lone bishop in corner 1") {
    val bishop = Bishop(Black, Square('a', 1))
    val cb = ChessBoard()
      .addPiece(bishop)
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 7)
    val rows = List.range(2, cb.dimension)
    val cols = List.range('b', 'h')
    cols.zip(rows).foreach(
      (r, c) => assert(moves.contains(Square(r, c)))
    )
  }

/*  

  test("Bishop: legalMoves works for lone bishop in corner 2") {
    val bishop = Bishop(White, Square('a', 8))
    val cb = ChessBoard()
      .addPiece(bishop)
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 1 to cb.dimension-1 do
      assert(moves.contains(Square('a', i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, cb.dimension)))
  }

  test("Bishop: legalMoves works for lone bishop in corner 3") {
    val bishop = Bishop(White, Square('h', 8))
    val cb = ChessBoard()
      .addPiece(bishop)
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 1 to cb.dimension - 1 do
      assert(moves.contains(Square('h', i)))
    for c <- 'a' to 'g' do
      assert(moves.contains(Square(c, cb.dimension)))
  }

  test("Bishop: legalMoves works for lone bishop in corner 4") {
    val bishop = Bishop(White, Square('h', 1))
    val cb = ChessBoard()
      .addPiece(bishop)
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 2 to cb.dimension do
      assert(moves.contains(Square('h', i)))
    for c <- 'a' to 'g' do
      assert(moves.contains(Square(c, 1)))
  }

  test("Bishop: legalMoves works with multiple bishops same colour") {
    val bishop = Bishop(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('e', 6)))
      .addPiece(Bishop(White, Square('e', 3)))
      .addPiece(Bishop(White, Square('a', 4)))
      .addPiece(Bishop(White, Square('g', 4)))
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 5)
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('b', 4)))
    assert(moves.contains(Square('c', 4)))
    assert(moves.contains(Square('d', 4)))
    assert(moves.contains(Square('f', 4)))
  }

  test("Bishop: legalMoves works with single bishop same colour 1") {
    val COL = 'e'
    val ROW = 4
    val bishop = Bishop(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('e', 6)))
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 11)
    assert(moves.contains(Square(COL, 5)))
    for i <- 1 to 3 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'a' to 'd' do
      assert(moves.contains(Square(c, ROW)))
    for c <- 'f' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }

  test("Bishop: legalMoves works with single bishop same colour 2") {
    val COL = 'a'
    val ROW = 1
    val bishop = Bishop(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('a', 8)))
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 13)
    for i <- 2 to 7 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }


  test("Bishop: legalMoves works with single bishop different colour 1") {
    val COL = 'e'
    val ROW = 4
    val bishop = Bishop(White, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(Black, Square('e', 6)))
    val moves = bishop.legalMoves(cb)
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

  test("Bishop: legalMoves works with single bishop different colour 2") {
    val COL = 'a'
    val ROW = 1
    val bishop = Bishop(Black, Square(COL, ROW))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('a', 8)))
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 14)
    for i <- 2 to 8 do
      assert(moves.contains(Square(COL, i)))
    for c <- 'b' to 'h' do
      assert(moves.contains(Square(c, ROW)))
  }

  test("Bishop: legalMoves works with multiple bishops opposite colour") {
    val bishop = Bishop(Black, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('e', 6)))
      .addPiece(Bishop(White, Square('e', 3)))
      .addPiece(Bishop(White, Square('a', 4)))
      .addPiece(Bishop(White, Square('g', 4)))
    val moves = bishop.legalMoves(cb)
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

  test("Bishop: legalMoves works with multiple bishops various colour") {
    val bishop = Bishop(White, Square('e', 4))
    val cb = ChessBoard()
      .addPiece(bishop)
      .addPiece(Bishop(White, Square('e', 6)))
      .addPiece(Bishop(Black, Square('e', 3)))
      .addPiece(Bishop(White, Square('a', 4)))
      .addPiece(Bishop(Black, Square('g', 4)))
    val moves = bishop.legalMoves(cb)
    assert(moves.length === 7)
    assert(moves.contains(Square('e', 5)))
    assert(moves.contains(Square('e', 3)))
    assert(moves.contains(Square('b', 4)))
    assert(moves.contains(Square('c', 4)))
    assert(moves.contains(Square('d', 4)))
    assert(moves.contains(Square('f', 4)))
    assert(moves.contains(Square('g', 4)))
  }
*/
}
