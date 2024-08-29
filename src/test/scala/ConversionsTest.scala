import org.scalatest.funsuite.AnyFunSuite

class ConversionsSuite extends AnyFunSuite {
  test("Part1: test charToIndex") {
    assert(Conversions.charToIndex('a') === 1)
    assert(Conversions.charToIndex('b') === 2)
    assert(Conversions.charToIndex('g') === 7)
    assert(Conversions.charToIndex('h') === 8)
  }

  test("Part1: test indexToChar") {
    assert(Conversions.indexToChar(1) === 'a')
    assert(Conversions.indexToChar(2) === 'b')
    assert(Conversions.indexToChar(7) === 'g')
    assert(Conversions.indexToChar(8) === 'h')
  }
}
