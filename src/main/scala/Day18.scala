import util.Day

import scala.annotation.tailrec

object Day18 extends Day(18):
  override def solve(): Unit =
    val scan = inputLines.map(_.strip.split(",")).map(l => (l.head.toInt, l(1).toInt, l(2).toInt))

    //Part 1
    println(scan.map(c => neighbors(c).count(n => !scan.contains(n))).sum)

    //Part 2
    println(scan.flatMap(c => neighbors(c).toList).count(n => !scan.contains(n) && !trapped(scan.toSet, n)))


  def neighbors(pos: (Int, Int, Int)): Set[(Int, Int, Int)] =
    (for
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if (Math.abs(x) + Math.abs(y) + Math.abs(z)) == 1
    yield (pos._1 + x, pos._2 + y, pos._3 + z)).toSet

  def trapped(scan: Set[(Int, Int, Int)], pos: (Int, Int, Int)): Boolean =
    val maxX = scan.maxBy(_._1)._1
    val maxY = scan.maxBy(_._2)._2
    val maxZ = scan.maxBy(_._3)._3
    @tailrec
    def trappedH(curr: Set[(Int, Int, Int)], found: Set[(Int, Int, Int)]): Boolean =
      val next = curr.flatMap(neighbors).filter(!found.contains(_)).filter(c =>
        !scan.contains(c) && c._1 >= -1 && c._2 >= -1 && c._3 >= -1 && c._1 <= maxX + 1 && c._2 <= maxY + 1 && c._3 <= maxZ + 1
      )

      if next.isEmpty then
        true
      else if next.contains((0, 0, 0)) then
        false
      else
        trappedH(next, found ++ curr)
    trappedH(Set(pos), Set())

