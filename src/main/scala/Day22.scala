import util.Day

import scala.annotation.tailrec

object Day22 extends Day(22):
  override def solve(): Unit =
    val split = rawInput.split("\n\n")
    val map = split.head
    val instr = split(1).strip

    val lines = map.split("\n").toList
    val maxLength = lines.maxBy(_.length).length
    val parsedMap = lines.indices.flatMap(col => lines.head.padTo(maxLength, ' ').indices.map(row => {
      (col, row) -> lines(col).padTo(maxLength, ' ')(row)
    })).toMap.filter(e => e._2 != ' ')
    val parsedInstructions = parseInstructions(instr)

    val (p, d) = move(parsedMap, parsedInstructions)

    val facingScore = d match
      case (0, 1) => 0
      case (1, 0) => 1
      case (-1, 0) => 3
      case (0, -1) => 2

    val (y, x) = p

    //Part 1
    println(1000*(y + 1) + 4*(x + 1) + facingScore)

    val (yc, xc, fc) = moveCube(parsedMap, parsedInstructions)

    val facingScoreC = fc match
      case '>' => 0
      case 'v' => 1
      case '<' => 2
      case '^' => 3

    //Part 2
    println(1000*(yc + 1) + 4*(xc + 1) + facingScoreC)

  def moveCube(map: Map[(Int, Int), Char], instructions: List[String]): (Int, Int, Char) =
    val next = buildEdgeMap()
    def rotate(curr: Char, action: String): Char = action match
      case "L" => curr match
        case '>' => '^'
        case '<' => 'v'
        case '^' => '<'
        case 'v' => '>'
      case "R" => curr match
        case '>' => 'v'
        case '<' => '^'
        case '^' => '>'
        case 'v' => '<'

    @tailrec
    def move(pos: (Int, Int), direction: Char, distance: Int): (Int, Int, Char) =
      if distance == 0 then
        (pos._1, pos._2, direction)
      else
        val np =
          if direction == '>' then
            (pos._1, pos._2 + 1)
          else if direction == '<' then
            (pos._1, pos._2 - 1)
          else if direction == '^' then
            (pos._1 - 1, pos._2)
          else
            (pos._1 + 1, pos._2)
        if !map.contains(np) then
          val wrap = next((pos._1, pos._2, direction))
          if map((wrap._1, wrap._2)) == '#' then
            (pos._1, pos._2, direction)
          else
            move((wrap._1, wrap._2), wrap._3, distance - 1)
        else if map(np) == '#' then
          (pos._1, pos._2, direction)
        else
          move(np, direction, distance - 1)

    val startY = map.keys.minBy(_._1)._1
    val startX = map.filter(e => e._1._1 == startY).minBy(_._1._2)._1._2
    instructions.grouped(2).foldLeft((startY, startX, '>'))((acc, nextI) =>
      val afterMove = move((acc._1, acc._2), acc._3, nextI.head.toInt)

      val nextD = if nextI.size == 2 then rotate(afterMove._3, nextI(1)) else afterMove._3

      (afterMove._1, afterMove._2, nextD)
    )

  def buildEdgeMap(): Map[(Int, Int, Char), (Int, Int, Char)] =
    val uu = (50 until 100).map(x => (0, x, '^') -> (100 + x, 0, '>')) //c
    val ul = (0 until 50).map(y => (y, 50, '<') -> (149 - y, 0, '>')) //c

    val ru = (100 until 150).map(x => (0, x, '^') -> (199, x - 100, '^')) //c
    val rr = (0 until 50).map(y => (y, 149, '>') -> (149 - y, 99,'<')) //c
    val rd = (100 until 150).map(x => (49, x, 'v') -> (x - 50, 99, '<')) //c

    val fl = (50 until 100).map(y => (y, 50, '<') -> (100, y - 50, 'v')) //c
    val fr = (50 until 100).map(y => (y, 99, '>') -> (49, y + 50, '^')) //c

    val dr = (100 until 150).map(y => (y, 99, '>') -> (149 - y, 149, '<')) //c
    val dd = (50 until 100).map(x => (149, x, 'v') -> (100 + x, 49, '<')) //c

    val ll = (100 until 150).map(y => (y, 0, '<') -> (149 - y, 50, '>')) //c
    val lu = (0 until 50).map(x => (100, x, '^') -> (x + 50, 50, '>'))  //c

    val bl = (150 until 200).map(y => (y, 0, '<') -> (0, y - 100, 'v')) //c
    val br = (150 until 200).map(y => (y, 49, '>') -> (149, y - 100, '^')) //c
    val bd = (0 until 50).map(x => (199, x, 'v') -> (0, x + 100, 'v')) //c

    (uu ++ ul ++ ru ++ rr ++ rd ++ fl ++ fr ++ dr ++ dd ++ ll ++ lu ++ bl ++ br ++ bd).toMap




  def move(map: Map[(Int, Int), Char], instructions: List[String]): ((Int, Int), (Int, Int)) =
    def rotate(currDir: (Int, Int), rot: String): (Int, Int) = rot match
      case "R" => (currDir._2, currDir._1 * (-1))
      case "L" => (currDir._2 * (-1), currDir._1)

    @tailrec
    def moveDistance(pos: (Int, Int), direction: (Int, Int), distance: Int): (Int, Int) = distance match
      case 0 => pos
      case _ =>
        if direction._1 != 0 then //Y - Movement
          val nY = pos._1 + direction._1
          if map.contains((nY, pos._2)) then
            if map((nY, pos._2)) == '.' then
              moveDistance((nY, pos._2), direction, distance - 1)
            else
              pos
          else
            if direction._1 < 0 then
              val aY = map.filter(e => e._1._2 == pos._2).maxBy(_._1._1)._1._1
              if map((aY, pos._2)) == '.' then
                moveDistance((aY, pos._2), direction, distance - 1)
              else
                pos
            else
              val aY = map.filter(e => e._1._2 == pos._2).minBy(_._1._1)._1._1
              if map((aY, pos._2)) == '.' then
                moveDistance((aY, pos._2), direction, distance - 1)
              else
                pos
        else // X - Movement
          val nX = pos._2 + direction._2
          if map.contains((pos._1, nX)) then
            if map((pos._1, nX)) == '.' then
              moveDistance((pos._1, nX), direction, distance - 1)
            else
              pos
          else
            if direction._2 < 0 then
              val aX = map.filter(e => e._1._1 == pos._1).maxBy(_._1._2)._1._2
              if map((pos._1, aX)) == '.' then
                moveDistance((pos._1, aX), direction, distance - 1)
              else
                pos
            else
              val aX = map.filter(e => e._1._1 == pos._1).minBy(_._1._2)._1._2
              if map((pos._1, aX)) == '.' then
                moveDistance((pos._1, aX), direction, distance - 1)
              else
                pos

    val startY = map.keys.minBy(_._1)._1
    val startX = map.filter(e => e._1._1 == startY).minBy(_._1._2)._1._2

    instructions.grouped(2).foldLeft(((startY, startX), (0, 1)))((acc, nextI) =>
      val p = acc._1
      val d = acc._2

      val nextP = moveDistance(p, d, nextI.head.toInt)
      val nextD = if nextI.size == 2 then rotate(d, nextI(1)) else d
      (nextP, nextD)
    )

  @tailrec
  def parseInstructions(instr: String, acc: List[String] = List()): List[String] =
    if instr.isEmpty then
      acc
    else if instr.head.isDigit then
      parseInstructions(instr.dropWhile(_.isDigit), acc.appended(instr.takeWhile(_.isDigit)))
    else
      parseInstructions(instr.drop(1), acc.appended(instr.take(1)))