import util.{Day, Util}

object Day4 extends Day(4):
  override def solve(): Unit =
    val ranges = raw"(\d+)-(\d+),(\d+)-(\d+)".r
    val input = inputLines.map { case ranges(s1, e1, s2, e2) => (s1.toInt, e1.toInt, s2.toInt, e2.toInt) }

    //Part 1
    println(input.count(contained))

    //Part 2
    println(input.count(overlap))

  def contained(ls: Int, le: Int, rs: Int, re: Int): Boolean =
    (ls >= rs && le <= re) || (rs >= ls && re <= le)

  def overlap(ls: Int, le: Int, rs: Int, re: Int): Boolean =
    (rs <= ls && ls <= re) || (ls <= rs && rs <= le)