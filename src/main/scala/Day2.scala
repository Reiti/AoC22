import util.{Day, Util}

object Day2 extends Day(2):
  override def solve(): Unit =
    val input = inputLines.map(_.split(" ")).map(s => (s(0).charAt(0), s(1).charAt(0)))

    //Part 1
    println(input.map(score).sum)

    //Part 2
    println(input.map(p => score((p._1, move(p._1, p._2)))).sum)

  def score(pair: (Char, Char)): Int = pair match
    case (o, 'X') => 1 + (if o == 'C' then 6 else if o == 'A' then 3 else 0)
    case (o, 'Y') => 2 + (if o == 'A' then 6 else if o == 'B' then 3 else 0)
    case (o, 'Z') => 3 + (if o == 'B' then 6 else if o == 'C' then 3 else 0)
    case _ => 0

  val moves: Array[Char] = Array('X', 'Y', 'Z')
  val shift: Array[Int] = Array(2, 0, 1)

  def move(o: Char, r: Char): Char =
    moves((shift(r - 'X') + o - 'A') % 3)