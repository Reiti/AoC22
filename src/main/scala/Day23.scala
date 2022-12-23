import util.{Day, Util}

import scala.annotation.tailrec

object Day23 extends Day(23):
  override def solve(): Unit =
    val elves = inputMap.filter(e => e._2 == '#').keys.toSet

    //Part 1
    println(emptySpace(simulate(elves, 10)))

    //Part 2
    println(findEquilibrium(elves))

  @tailrec
  def findEquilibrium(elves: Set[(Int, Int)], round: Int = 0, dirs: List[String] = List("N", "S", "W", "E")): Int =
    val next = step(elves, dirs)

    if next == elves then
      round + 1
    else
      findEquilibrium(next, round + 1, dirs.tail.appended(dirs.head))

  @tailrec
  def simulate(elves: Set[(Int, Int)], rounds: Int, dirs: List[String] = List("N", "S", "W", "E")): Set[(Int, Int)] =
    if rounds == 0 then
      elves
    else
      simulate(step(elves, dirs), rounds - 1, dirs.tail.appended(dirs.head))

  def move(elves: Set[(Int, Int)], e: (Int, Int), direction: String): Option[(Int, Int)] =
    if Util.mooreNeighborhood.forall(d => !elves.contains((e._1 + d._1, e._2 + d._2))) then
      None
    else
      direction match
      case "N" =>
        if !elves.contains((e._1 - 1, e._2 - 1)) && !elves.contains((e._1 - 1, e._2)) && !elves.contains((e._1 - 1, e._2 + 1)) then
          Some((e._1 - 1, e._2))
        else
          None
      case "S" =>
        if !elves.contains((e._1 + 1, e._2 - 1)) && !elves.contains((e._1 + 1, e._2)) && !elves.contains((e._1 + 1, e._2 + 1)) then
          Some((e._1 + 1, e._2))
        else
          None
      case "E" =>
        if !elves.contains((e._1 - 1, e._2 + 1)) && !elves.contains((e._1, e._2 + 1)) && !elves.contains((e._1 + 1, e._2 + 1)) then
          Some((e._1, e._2 + 1))
        else
          None
      case "W" =>
        if !elves.contains((e._1 - 1, e._2 - 1)) && !elves.contains((e._1, e._2 - 1)) && !elves.contains((e._1 + 1, e._2 - 1)) then
          Some((e._1, e._2 - 1))
        else
          None

  def step(elves: Set[(Int, Int)], dirs: List[String]): Set[(Int, Int)] =
    val proposals = elves.map(e => dirs.flatMap(d => move(elves, e, d)).headOption match
      case Some(p) =>
        e -> p
      case None =>
        e -> e
    )

    val counts = proposals.groupBy(_._2).map(e => e._1 -> e._2.size)
    proposals.map(p =>
      if counts(p._2) == 1 then
        p._2
      else
        p._1
    )


  def emptySpace(elves: Set[(Int, Int)]): Int =
    val minY = elves.minBy(_._1)._1
    val maxY = elves.maxBy(_._1)._1
    val minX = elves.minBy(_._2)._2
    val maxX = elves.maxBy(_._2)._2

    (minY to maxY).flatMap(y =>
      (minX to maxX).map(x =>
        if elves.contains((y, x)) then
          0
        else
          1
      )
    ).sum