import util.Util

object Day4:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayLines(4).map(_.split(",").map(_.split("-").map(_.toInt)))

    //Part 1
    println(input.count(l => contained(l.head, l.last)))

    //Part 2
    println(input.count(l => overlap(l.head, l.last)))

  def contained(l: Array[Int], r: Array[Int]): Boolean =
    (l.head >= r.head && l.last <= r.last) || (r.head >= l.head && r.last <= l.last)

  def overlap(l: Array[Int], r: Array[Int]): Boolean =
    (r.head <= l.head && l.head <= r.last) || (l.head <= r.head && r.head <= l.last)