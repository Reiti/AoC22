import util.Util

object Day6:
  def main(args: Array[String]): Unit =
    val input = Util.loadDay(6)

    //Part 1
    println(input.sliding(4).zipWithIndex.find(_._1.distinct.length == 4).get._2 + 4)

    //Part 2
    println(input.sliding(14).zipWithIndex.find(_._1.distinct.length == 14).get._2 + 14)