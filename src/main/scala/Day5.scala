import util.Util

import scala.annotation.tailrec

object Day5:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayKeepWhitespace(5).split("\n\n")

    val stacks = input.head.split("\n")
      .map(_.toArray).transpose
      .map(_.mkString.strip)
      .filter(l => !(l.startsWith("[") || l.startsWith("]") || l.isBlank))
      .map(_.reverse)
      .map(line => (line.head.asDigit, line.tail))
      .toMap

    val instr = raw"move (\d+) from (\d+) to (\d+)".r
    val instructions = input.last.split("\n")
      .map { case instr(count, src, dest) => (count.toInt, src.toInt, dest.toInt) }
      .toList

    //Part 1
    println(execute(stacks, instructions, true).toList.sortBy(_._1).map(_._2.reverse.head).mkString)

    //Part 2
    println(execute(stacks, instructions, false).toList.sortBy(_._1).map(_._2.reverse.head).mkString)

  @tailrec
  def execute(stacks: Map[Int, String], instructions: List[(Int, Int, Int)], reverse: Boolean): Map[Int, String] = instructions match
    case (count, src, dest) :: rest => execute(step(stacks, count, src, dest, reverse), rest, reverse)
    case _ => stacks

  def step(stacks: Map[Int, String], count: Int, src: Int, dest: Int, reverse: Boolean): Map[Int, String] =
    val taken = stacks(src).takeRight(count)
    val newSrc = stacks(src).dropRight(count)
    val newDest = stacks(dest).appended(if reverse then taken.reverse else taken).mkString
    stacks.updated(src, newSrc).updated(dest, newDest)