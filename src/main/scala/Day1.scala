import util.Day

object Day1 extends Day(1):
  override def solve(): Unit =
    val calories = input.split("\n\n").map(_.split("\n").map(_.toInt).sum).sorted(Ordering[Int].reverse)

    //Part 1
    println(calories.head)

    //Part 2
    println(calories.take(3).sum)