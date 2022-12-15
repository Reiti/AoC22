import util.{Day, Util}

import scala.annotation.tailrec

object Day14 extends Day(14):
  override def solve(): Unit =
    val cave: Map[(Int, Int), Char] = parse(inputLines)

    //Part 1
    println(countDrops(cave, dropsToInfinity))

    //Part 2
    println(countDrops(cave.withDefault(k => defaultWithFloor(k, cave.keys.maxBy(_._2)._2 + 2)), sourceBlocked))

  @tailrec
  def parse(input: List[String], acc: Map[(Int, Int), Char] = Map().withDefaultValue(' ')): Map[(Int, Int), Char] = input match
    case x :: xs =>
      parse(xs, acc ++ x.split(" -> ").map(_.split(",")).map(s => (s.head.toInt, s(1).toInt)).sliding(2).flatMap(s => drawLine(s.head, s(1))).map(c => c -> '#').toMap)
    case _ => acc

  def drawLine(start: (Int, Int), end: (Int, Int)): Set[(Int, Int)] =
    if start._1 == end._1 then
      val (s, e) = if start._2 <= end._2 then (start._2, end._2) else (end._2, start._2)
      (s to e).map(y => (start._1, y)).toSet
    else
      val (s, e) = if start._1 <= end._1 then (start._1, end._1) else (end._1, start._1)
      (s to e).map(x => (x, start._2)).toSet

  @tailrec
  def countDrops(cave: Map[(Int, Int), Char], stopCondition: (Map[(Int, Int), Char], (Int, Int)) => Boolean, acc: Int = 0): Int = dropSand(cave, stopCondition) match
    case (true, newCave) => countDrops(newCave, stopCondition, acc + 1)
    case (false, _) => acc

  def dropSand(cave: Map[(Int, Int), Char], stopCondition: (Map[(Int, Int), Char], (Int, Int)) => Boolean): (Boolean, Map[(Int, Int), Char]) =
    @tailrec
    def dropSandH(sand: (Int, Int)): (Boolean, Map[(Int, Int), Char]) =
      if stopCondition(cave, sand) then
        (false, cave)
      else if cave((sand._1, sand._2 + 1)) == ' ' then
        dropSandH((sand._1, sand._2 + 1))
      else if cave((sand._1 - 1, sand._2 + 1)) == ' ' then
        dropSandH((sand._1 - 1, sand._2 + 1))
      else if cave((sand._1 + 1, sand._2 + 1)) == ' ' then
        dropSandH((sand._1 + 1, sand._2 + 1))
      else
        (true, cave + (sand -> '+'))

    dropSandH((500, 0))

  def defaultWithFloor(key: (Int, Int), floor: Int): Char = if key._2 >= floor then '#' else ' '

  def dropsToInfinity(cave: Map[(Int, Int), Char], sand: (Int, Int)): Boolean = sand._2 > cave.keys.maxBy(_._2)._2

  def sourceBlocked(cave: Map[(Int, Int), Char], sand: (Int, Int)): Boolean = cave((500, 0)) != ' '