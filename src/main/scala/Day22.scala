import util.Day

import scala.annotation.tailrec

object Day22 extends Day(22):
  override def solve(): Unit =
    val split = rawInput.split("\n\n")
    //val split = List("        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.", "10R5L5R10L4R5L5");

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

  def printMap(map: Map[(Int, Int), Char]): Unit =
    def minY = map.keys.minBy(_._1)._1
    def minX = map.keys.minBy(_._2)._2
    def maxY = map.keys.maxBy(_._1)._1
    def maxX = map.keys.maxBy(_._2)._2

    (minY to maxY).foreach(y =>
      (minX to maxX).foreach(x =>
        print(map.getOrElse((y, x), ' '))
      )
      println
    )