import util.Util

object Day3:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayLines(3)

    //Part 1
    println(input.map(line => line.splitAt(line.length/2)).map((l, r) => score(l.intersect(r).head)).sum)

    //Part 2
    println(input.grouped(3).map(g => score(g.fold(g.head)((l, r) => l.intersect(r)).head)).sum)

  def score(c: Char): Int =
    if c <= 'Z' then
      c - 'A' + 27
    else
      c - 'a' + 1