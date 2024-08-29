
object Conversions:
  def charToIndex(c: Char) =
    c.toInt - 'a'.toInt + 1

  def indexToChar(i: Int) =
    ('a'.toInt + i - 1).toChar
