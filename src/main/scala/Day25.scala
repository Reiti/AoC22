import util.Day

import scala.annotation.tailrec

object Day25 extends Day(25):
  override def solve(): Unit =
    val parsed = inputLines.map(_.reverse).map(s => s.map(c =>
      if c == '=' then -2
      else if c == '-' then -1
      else c.asDigit
    ).toList)

    val snafus = parsed.map(l => l.padTo(parsed.map(_.length).max, 0))

    //Part 1
    println(add(snafus.transpose).map(n =>
      if n == -2 then '='
      else if n == -1 then '-'
      else (n + '0').toChar
    ).mkString)

  @tailrec
  def add(snafus: List[List[Int]], sum: List[Int] = List()): List[Int] = snafus match
    case x :: xs =>
      val s = x.sum
      val (n, c) = move(s, if s < 0 then 1 else -1, 0)
      val carry = if s < 0 then c * (-1) else c
      add(
        if c != 0 then
          if xs.nonEmpty then
            xs.tail.prepended(xs.head.appended(carry))
          else
            List(List(carry))
        else
          xs
        , sum.prepended(n))
    case _ => sum

  @tailrec
  def move(start: Int, direction: Int, times: Int): (Int, Int) =
    if start >= -2 && start <= 2 then
      (start, times)
    else
      move(start + (5 * direction), direction, times + 1)
