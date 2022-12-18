import util.{Day, Util}

import scala.annotation.tailrec

case class Shape(shape: Set[(Int, Int)]):
  val height: Int = shape.map(_._2).max

  def at(x: Int, y: Int): Set[(Int, Int)] =
    shape.map(p => (p._1 + x, y - p._2))

  def collision(xPos: Int, yPos: Int, map: Set[(Int, Int)]): Boolean =
    val t = at(xPos, yPos).exists(p =>
      p._1 < 0 || p._1 > 6 || p._2 < 0 || map.contains((p._1, p._2))
    )

    //if t then
    //  println(s"${at(xPos, yPos).find(_._1 < 0)} - ${at(xPos, yPos).find(_._1 > 6)} - ${at(xPos, yPos).find(_._2 < 0)} - ${at(xPos, yPos).find(map.contains)}")
    t

object Day17 extends Day(17):
  override def solve(): Unit =
    val flat = Shape(Set((0, 0), (1, 0), (2, 0), (3, 0)))
    val cross = Shape(Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)))
    val l = Shape(Set((2, 0), (2, 1), (0, 2), (1, 2), (2, 2)))
    val i = Shape(Set((0, 0), (0, 1), (0, 2), (0, 3)))
    val block = Shape(Set((0, 0), (0, 1), (1, 0), (1, 1)))

    val shapes = Array(flat, cross, l, i, block)

    val testInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

    val processed = input.split("").zipAll(List("v"), "v", "v").map(e => e._1 + e._2).mkString

    //Part 1
    Util.time {
        //printMap(play(processed.toCharArray, shapes, 2)._1)
      println(play(processed.toCharArray, shapes, processed.length * 100)._2)
    }


    /*
    val heights = play(LazyList.continually(input.toCharArray.toList).flatten, shapes.take(4000).toList)._1

    val tests = (1 to heights.size).view.flatMap(n => (0 to n).map(offset => (n, offset))).filter((n, offset) => (heights.length - offset) % n == 0)

    val repeats = tests.filter((n, offset) =>
      val l = heights.drop(offset).grouped(n).map(_.sum).toList
      if l.size >= 2 then
        l.sliding(2).forall(e => e.head == e(1))
      else
        false
    )

    /*
    val (n, offset) = repeats.head

    val start = heights.take(offset)
    val repeating = heights.slice(offset, offset + n)
    */

    println(repeats.map((n, offset) =>
      val start = heights.take(offset)
      val repeating = heights.slice(offset, offset + n)
      heightAfter(BigInt("1000000000000"), start, repeating)
    ).max)
    */



  def heightAfter(blocks: BigInt, start: List[Int], repeating: List[Int]): BigInt =
    if blocks <= start.length then
      start.take(blocks.toInt).sum
    else
      val tail = (blocks - BigInt(start.length)) % BigInt(repeating.length)
      val rep = (blocks - BigInt(start.length)) / BigInt(repeating.length)

      BigInt(start.sum) + rep*BigInt(repeating.sum) + BigInt(repeating.take(tail.toInt).sum)

  def play(jets: Array[Char], shapes: Array[Shape], stopAt: Int): (Set[(Int, Int)], Int) =
    @tailrec
    def playShape(posC: Int, posS: Int, x: Int, y: Int, map: Set[(Int, Int)], currMax: Int): (Set[(Int, Int)], Int) =
      val shape = shapes(posS % 5)
      val action = jets(posC % jets.length)

      if posS == stopAt then
        (map, currMax)
      else
        action match
        case '>' =>
          if shape.collision(x + 1, y, map) then
            playShape(posC + 1, posS, x, y, map, currMax)
          else
            playShape(posC + 1, posS, x + 1, y, map, currMax)
        case '<' =>
          if shape.collision(x - 1, y, map) then
            playShape(posC + 1, posS, x, y, map, currMax)
          else
            playShape(posC + 1, posS, x - 1, y, map, currMax)
        case 'v' =>
          if shape.collision(x, y - 1, map) then
            val h = shape.at(x, y).map(_._2).max + 1
            val newMax = if currMax >= h then currMax else h

            playShape(posC + 1, posS + 1, 2, newMax + shapes((posS + 1) % 5).height + 3, map ++ shape.at(x, y), newMax)
          else
            playShape(posC + 1, posS, x , y - 1, map, currMax)

    playShape(0, 0, 2, 3, Set[(Int, Int)](), 0)

  def hasLine(map: Set[(Int, Int)], at: Int): Boolean =
    (0 to 6).forall(x => map.contains((x, at)))


  def printMap(map: Set[(Int, Int)]): Unit =
    (0 to getHeight(map)).reverse.foreach(i =>
      (0 to 6).foreach(j =>
      if map.contains((j, i)) then print("#") else print("."))
      println
    )

  def getHeight(map: Set[(Int, Int)]): Int = if map.isEmpty then 0 else map.map(_._2).max + 1

