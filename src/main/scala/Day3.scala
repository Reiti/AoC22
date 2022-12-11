import util.{Day, Util}

object Day3 extends Day(3):
  override def solve(): Unit =
    //Part 1
    println(inputLines.map(line => line.splitAt(line.length/2)).map((l, r) => score(l.intersect(r).head)).sum)

    //Part 2
    println(inputLines.grouped(3).map(g => score(g.fold(g.head)((l, r) => l.intersect(r)).head)).sum)

  def score(c: Char): Int =
    if c <= 'Z' then
      c - 'A' + 27
    else
      c - 'a' + 1