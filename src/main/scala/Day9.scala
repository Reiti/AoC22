import util.Util

import scala.annotation.tailrec

object Day9:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayLines(9).flatMap{ case s"$dir $count" => List.fill(count.toInt)(dir) }

    //Part 1
    println(move(input, 1).size)

    //Part 2
    println(move(input, 9).size)

  def move(moves: List[String], tailSize: Int): Set[(Int, Int)] =
    @tailrec
    def moveH(head: (Int, Int), tail: List[(Int, Int)], moves: List[String], visited: Set[(Int, Int)]): Set[(Int, Int)] = moves match
      case x :: xs =>
        val newHead = x match
          case "R" => (head._1, head._2 + 1)
          case "L" => (head._1, head._2 - 1)
          case "U" => (head._1 + 1, head._2)
          case "D" => (head._1 - 1, head._2)
        moveH(newHead, step(newHead, tail), xs, visited + tail.last)
      case _ => visited + tail.last
    moveH((0, 0), List.fill(tailSize)((0, 0)), moves, Set())

  @tailrec
  def step(front: (Int, Int), tail: List[(Int, Int)], newTail: List[(Int, Int)] = List()): List[(Int, Int)] = tail match
    case next :: rest =>
      val xDist = front._1 - next._1
      val yDist = front._2 - next._2
      val newElem =
        if Math.abs(xDist) > 1 || Math.abs(yDist) > 1 then
          (next._1 + xDist.sign, next._2 + yDist.sign)
        else
          next
      step(newElem, rest, newTail.appended(newElem))
    case _ => newTail