import util.{Day, Util}

import scala.annotation.tailrec

case class Monkey(operation: Long => Long, test: Long => Int, mod: Long):
  def apply(v: Long, divide: Boolean): (Long, Int) =
    val nv = if divide then operation(v) / 3 else operation(v)
    (nv, test(nv))

object Day11 extends Day(11):
  override def solve(): Unit =
    val parsed = input.split("Monkey \\d:")
      .filter(_.nonEmpty)
      .map(_.strip.split("\n").map(_.strip))
      .map(parse)
    val inputs = parsed.map(_._2).zipWithIndex.map(z => (z._2, z._1)).toMap
    val monkeys = parsed.map(_._1).zipWithIndex.toList
    val modulo = monkeys.map(_._1.mod).product

    val (_, inspections) = (1 to 20).foldLeft((inputs, Map[Int, Long]()))((c, _) => round(monkeys, c._1, c._2, true))

    //Part 1
    println(inspections.toList.map(_._2).sorted.reverse.take(2).product)

    val (_, inspections2) = (1 to 10000).foldLeft((inputs, Map[Int, Long]()))((c, _) =>
      val (v, i) = round(monkeys, c._1, c._2, false)
      (truncate(v, modulo), i)
    )

    //Part 2
    println(inspections2.toList.map(_._2).sorted.reverse.take(2).product)

  def parse(input: Array[String]): (Monkey, List[Long]) =
    val start = input.head.split(": ")(1).split(", ").map(_.toLong).toList
    val op = input(1).split(": ").collectFirst {
      case s"new = old $operator $op2" => op2 match
        case "old" => operator match
          case "+" => (p: Long) => p + p
          case "*" => (p: Long) => p * p
        case _ => operator match
          case "+" => (p: Long) => p + op2.toInt
          case "*" => (p: Long) => p * op2.toInt
    }.get
    val te = input(2).split(": ").collectFirst { case s"divisible by $num" => num.toInt }.get
    val tr = input(3).split(": ").collectFirst { case s"throw to monkey $num" => num.toInt }.get
    val fa = input(4).split(": ").collectFirst { case s"throw to monkey $num" => num.toInt }.get
    val cond = (p: Long) => if (p % te) == 0L then tr else fa

    (Monkey(op, cond, te), start)

  def handleMonkey(values: Map[Int, List[Long]], monkey: Monkey, idx: Int, divide: Boolean): Map[Int, List[Long]] = values.getOrElse(idx, List()).foldLeft(values)((n, curr) =>
    val (value, passTo) = monkey(curr, divide)
    val ex = n.getOrElse(passTo, List())
    n + (passTo -> ex.appended(value))
  ).removed(idx)

  @tailrec
  def round(monkeys: List[(Monkey, Int)], values: Map[Int, List[Long]], inspects: Map[Int, Long] = Map(), divide: Boolean): (Map[Int, List[Long]], Map[Int, Long]) = monkeys match
    case (monkey, idx) :: xs =>
      val count = values.getOrElse(idx, List()).size.toLong
      val curr = inspects.getOrElse(idx, 0L)
      round(xs, handleMonkey(values, monkey, idx, divide), inspects + (idx -> (curr + count)), divide)
    case _ => (values, inspects)

  def truncate(values: Map[Int, List[Long]], modulo: Long): Map[Int, List[Long]] = values.toList.map(e =>
    (e._1, e._2.map(_ % modulo))
  ).toMap







