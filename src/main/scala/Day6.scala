import util.{Day, Util}

object Day6 extends Day(6):
  override def solve(): Unit =
    //Part 1
    println(input.sliding(4).zipWithIndex.find(_._1.distinct.length == 4).get._2 + 4)

    //Part 2
    println(input.sliding(14).zipWithIndex.find(_._1.distinct.length == 14).get._2 + 14)