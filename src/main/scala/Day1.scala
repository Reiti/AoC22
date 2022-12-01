import util.Util

object Day1:
  def main(args: Array[String]): Unit =
    val input = Util.loadDay(1).split("\n\n").map(_.split("\n").map(_.toInt).sum).sorted(Ordering[Int].reverse)

    //Part 1
    println(input.head)

    //Part 2
    println(input.take(3).sum)