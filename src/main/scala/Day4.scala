import util.Util

object Day4:
  def main(args: Array[String]): Unit =
    val ranges = raw"(\d*)-(\d*),(\d*)-(\d*)".r
    val input = Util.loadDayLines(4).map { case ranges(s1, e1, s2, e2) => (s1.toInt, e1.toInt, s2.toInt, e2.toInt) }

    //Part 1
    println(input.count(contained))

    //Part 2
    println(input.count(overlap))

  def contained(ls: Int, le: Int, rs: Int, re: Int): Boolean =
    (ls >= rs && le <= re) || (rs >= ls && re <= le)

  def overlap(ls: Int, le: Int, rs: Int, re: Int): Boolean =
    (rs <= ls && ls <= re) || (ls <= rs && rs <= le)
