import util.{Day, Util}

import scala.annotation.tailrec

case class Shape(shape: Set[(Int, Int)]):
  val height: Int = shape.map(_._2).max

  def at(x: Int, y: Int): Set[(Int, Int)] =
    shape.map(p => (p._1 + x, y - p._2))

  def collision(xPos: Int, yPos: Int, map: Set[(Int, Int)]): Boolean =
    at(xPos, yPos).exists(p =>
      p._1 < 0 || p._1 > 6 || p._2 < 0 || map.contains((p._1, p._2))
    )

object Day17 extends Day(17):
  override def solve(): Unit =
    val flat = Shape(Set((0, 0), (1, 0), (2, 0), (3, 0)))
    val cross = Shape(Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)))
    val l = Shape(Set((2, 0), (2, 1), (0, 2), (1, 2), (2, 2)))
    val i = Shape(Set((0, 0), (0, 1), (0, 2), (0, 3)))
    val block = Shape(Set((0, 0), (0, 1), (1, 0), (1, 1)))

    val shapes = Array(flat, cross, l, i, block)

    val processed = input.split("").zipAll(List("v"), "v", "v").map(e => e._1 + e._2).mkString

    //Part 1
    println(play(0, 0, processed.toCharArray, shapes, 2022, Set())._2)

    val want = BigInt("1000000000000")
    val target = want - BigInt(1846)

    val (_, init, _) = play(0, 0, processed.toCharArray, shapes, 1846, Set())

    val times = target / 1725
    val tail = target % 1725

    val (_, rep, _) = play(0, 0, processed.toCharArray, shapes, 1846 + 1725, Set())

    val diff = BigInt(rep - init)

    val (_, t, _) = play(0, 0, processed.toCharArray, shapes, 1846 + tail.toInt, Set())

    val tl = BigInt(t - init)

    //Part 2
    println(times * diff + init + tl)

  def play(startC: Int, startS: Int, jets: Array[Char], shapes: Array[Shape], stopAt: Int, map: Set[(Int, Int)]): (Set[(Int, Int)], Int, Vector[Int]) =
    @tailrec
    def playShape(posC: Int, posS: Int, x: Int, y: Int, map: Set[(Int, Int)], currMax: Int, lines: Set[(Set[Int], Int, Int)], heights: Vector[Int]): (Set[(Int, Int)], Int, Vector[Int]) =
      val shape = shapes(posS % 5)
      val action = jets(posC % jets.length)

      if posS == stopAt then
        (map, currMax, heights)
      else
        action match
        case '>' =>
          if shape.collision(x + 1, y, map) then
            playShape(posC + 1, posS, x, y, map, currMax, lines, heights)
          else
            playShape(posC + 1, posS, x + 1, y, map, currMax, lines, heights)
        case '<' =>
          if shape.collision(x - 1, y, map) then
            playShape(posC + 1, posS, x, y, map, currMax, lines, heights)
          else
            playShape(posC + 1, posS, x - 1, y, map, currMax, lines, heights)
        case 'v' =>
          if shape.collision(x, y - 1, map) then
            val h = shape.at(x, y).map(_._2).max + 1
            val newMax = if currMax >= h then currMax else h
            val newLine = (getLine(map ++ shape.at(x, y), newMax - 1), (posS + 1) % 5, (posC + 1) % jets.length)

            /*
            if countTops(map ++ shape.at(x, y)) > countTops(map) then
              println("top: " + (posS + 1) % 5 + " - " + (posC + 1) % jets.length + " - " + newMax)
            */

            playShape(posC + 1, posS + 1, 2, newMax + shapes((posS + 1) % 5).height + 3, map ++ shape.at(x, y), newMax, lines + newLine, heights.appended(newMax - currMax))
          else
            playShape(posC + 1, posS, x , y - 1, map, currMax, lines, heights)

    playShape(startC, startS, 2, 3, map, 0, Set(), Vector())

  def getLine(map: Set[(Int, Int)], at: Int): Set[Int] =
    (0 to 6).map(x => (x, at)).filter(e => map.contains(e)).map(e => e._1).toSet

  def countTops(map: Set[(Int, Int)]): Int =
    val present = List((0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (0, -1), (1, -1), (2, -1), (3, -1), (4, -1))
    val notPresent = List((5, -1), (6, -1))

    map.count(e => present.forall(p => map.contains(e._1 + p._1, e._2 + p._2) && notPresent.forall(np => !map.contains(e._1 + np._1, e._2 + np._2))))