import util.{Day, Util}

import scala.annotation.tailrec

object Day14 extends Day(14):
  override def solve(): Unit =
    val blocked: Set[(Int, Int)] = parse(inputLines)

    //Part 1
    println(countDrops(blocked, dropsToInfinity, isFree))

    //Part 2
    println(countDrops(blocked, sourceBlocked, isFreeWithFloor(blocked.maxBy(_._2)._2 + 2)))

  @tailrec
  def parse(input: List[String], acc: Set[(Int, Int)] = Set()): Set[(Int, Int)] = input match
    case x :: xs =>
      parse(xs, acc ++ x.split(" -> ").map(_.split(",")).map(s => (s.head.toInt, s(1).toInt)).sliding(2).flatMap(s => drawLine(s.head, s(1))))
    case _ => acc

  def drawLine(start: (Int, Int), end: (Int, Int)): Set[(Int, Int)] =
    if start._1 == end._1 then
      val (s, e) = if start._2 <= end._2 then (start._2, end._2) else (end._2, start._2)
      (s to e).map(y => (start._1, y)).toSet
    else
      val (s, e) = if start._1 <= end._1 then (start._1, end._1) else (end._1, start._1)
      (s to e).map(x => (x, start._2)).toSet

  @tailrec
  def countDrops(blocked: Set[(Int, Int)], stopCondition: (Set[(Int, Int)], (Int, Int)) => Boolean, free: (Set[(Int, Int)], (Int, Int)) => Boolean, acc: Int = 0): Int = dropSand(blocked, stopCondition, free) match
    case (true, newBlocked) => countDrops(newBlocked, stopCondition, free, acc + 1)
    case (false, _) => acc

  def dropSand(blocked: Set[(Int, Int)], stopCondition: (Set[(Int, Int)], (Int, Int)) => Boolean, free: (Set[(Int, Int)], (Int, Int)) => Boolean): (Boolean, Set[(Int, Int)]) =
    @tailrec
    def dropSandH(sand: (Int, Int)): (Boolean, Set[(Int, Int)]) =
      if stopCondition(blocked, sand) then
        (false, blocked)
      else if free(blocked, (sand._1, sand._2 + 1)) then
        dropSandH((sand._1, sand._2 + 1))
      else if free(blocked, (sand._1 - 1, sand._2 + 1)) then
        dropSandH((sand._1 - 1, sand._2 + 1))
      else if free(blocked, (sand._1 + 1, sand._2 + 1)) then
        dropSandH((sand._1 + 1, sand._2 + 1))
      else
        (true, blocked + sand)

    dropSandH((500, 0))

  def isFree(blocked: Set[(Int, Int)], key: (Int, Int)): Boolean = !blocked.contains(key)

  def isFreeWithFloor(floor: Int)(blocked: Set[(Int, Int)], key: (Int, Int)): Boolean = if key._2 >= floor then false else !blocked.contains(key)

  def dropsToInfinity(blocked: Set[(Int, Int)], sand: (Int, Int)): Boolean = sand._2 > blocked.maxBy(_._2)._2

  def sourceBlocked(blocked: Set[(Int, Int)], sand: (Int, Int)): Boolean = blocked.contains((500, 0))