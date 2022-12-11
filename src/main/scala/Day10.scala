import util.{Day, Util}

import scala.annotation.tailrec

object Day10 extends Day(10):
  override def solve(): Unit =
    val values = run(inputLines)
    val target = Set(20, 60, 100, 140, 180, 220)

    //Part 1
    println(values.view.filterKeys(target.contains).map(e => e._1 * e._2).sum)

    //Part 2
    print(values).grouped(40).foreach(println)

  @tailrec
  def run(input: List[String], register: Int = 1, adding: Option[Int] = None, values: Map[Int, Int] = Map(), cycle: Int = 1): Map[Int, Int] = adding match
    case Some(v) => run(input, register + v, None, values + (cycle -> register), cycle + 1)
    case None => input match
      case x :: xs => x match
        case s"addx $v" => run(xs, register, Some(v.toInt), values + (cycle -> register), cycle + 1)
        case "noop" => run(xs, register, None, values + (cycle -> register), cycle + 1)
      case _ => values

  def overlaps(sprite: Int, pixel: Int): Boolean = Math.abs(pixel - sprite) <= 1

  @tailrec
  def print(values: Map[Int, Int], pixel: Int = 0, picture: String = ""): String = values.get(pixel + 1) match
    case Some(p) =>
      if overlaps(p, pixel % 40) then
        print(values, pixel + 1, picture + "#")
      else
        print(values, pixel + 1, picture + ".")
    case None => picture