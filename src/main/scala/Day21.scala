import util.Day

object Day21 extends Day(21):
  override def solve(): Unit =
    val monkeys = inputLines.map(_.split(": ")).map(s => s.head -> s(1)).toMap

    //Part 1
    println(eval(monkeys, "root"))

    val rootSplit = monkeys("root").split(" ")
    val corrected = monkeys.updated("humn", "x").updated("root", rootSplit.head + " = " + rootSplit(2))

    //Part 2
    println(evalPoly(corrected, "root")._1.head)

  def eval(monkeys: Map[String, String], monkey: String): Long = monkeys(monkey) match
    case s"$l $op $r" => op match
      case "+" => eval(monkeys, l) + eval(monkeys, r)
      case "-" => eval(monkeys, l) - eval(monkeys, r)
      case "*" => eval(monkeys, l) * eval(monkeys, r)
      case "/" => eval(monkeys, l) / eval(monkeys, r)
    case s"$v" => v.toLong

  def evalPoly(monkeys: Map[String, String], monkey: String): (Vector[BigInt], Vector[BigInt]) = monkeys(monkey) match
    case s"$l $op $r" => op match
      case "+" =>
        val (nl, dl) = evalPoly(monkeys, l)
        val (nr, dr) = evalPoly(monkeys, r)

        (add(multiply(nl, dr), multiply(nr, dl)), multiply(dl, dr))
      case "-" =>
        val (nl, dl) = evalPoly(monkeys, l)
        val (nr, dr) = evalPoly(monkeys, r)

        (sub(multiply(nl, dr), multiply(nr, dl)), multiply(dl, dr))
      case "*" =>
        val (nl, dl) = evalPoly(monkeys, l)
        val (nr, dr) = evalPoly(monkeys, r)

        (multiply(nl, nr), multiply(dl, dr))
      case "/" =>
        val (nl, dl) = evalPoly(monkeys, l)
        val (nr, dr) = evalPoly(monkeys, r)

        (multiply(nl, dr), multiply(nr, dl))
      case "=" =>
        val (nl, dl) = evalPoly(monkeys, l)
        val (nr, dr) = evalPoly(monkeys, r)

        val (n, _) = (sub(multiply(nl, dr), multiply(nr, dl)), multiply(dl, dr))

        val res = (BigInt(-1) * n.head)/n(1)
        (Vector(res), Vector(1))

    case s"$v" => if v.head.isDigit then (Vector(BigInt(v)), Vector(BigInt(1))) else (Vector(BigInt(0), BigInt(1)), Vector(BigInt(1)))

  def add(l: Vector[BigInt], r: Vector[BigInt]): Vector[BigInt] =
    l.zipAll(r, BigInt(0), BigInt(0)).map(_ + _)

  def sub(l: Vector[BigInt], r: Vector[BigInt]): Vector[BigInt] =
    l.zipAll(r, BigInt(0), BigInt(0)).map(_ - _)

  def multiply(l: Vector[BigInt], s: BigInt): Vector[BigInt] =
    l.map(_ * s)

  def multiply(l: Vector[BigInt], r: Vector[BigInt]): Vector[BigInt] =
    l.zipWithIndex.map(e =>
      multiply(Vector.fill(e._2)(BigInt(0)).appendedAll(r), e._1)
    ).reduce(add)