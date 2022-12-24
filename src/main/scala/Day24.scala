import util.{Day, Util}

import scala.annotation.tailrec

object Day24 extends Day(24):
  override def solve(): Unit =
    val blizzards = inputMap.filter(e => e._2 != '#' && e._2 != '.').map(e => (e._1._1 - 1, e._1._2 - 1, e._2)).toSet
    val startPos = (0, 1)
    val endX = inputMap.keys.maxBy(_._2)._2 - 1
    val endY = inputMap.keys.maxBy(_._1)._1
    val targetPos = (endY, endX)

    val (period, map) = buildBlizzardMap(blizzards, endX, endY - 1, Map(0 -> blizzards))

    val res = Util.bfs((startPos, 0), isTarget(targetPos), neighbors(endX, endY - 1, inputMap, map, period))

    //Part 1
    println(res.distance)

    val back = Util.bfs(res.path.last, isTarget(startPos), neighbors(endX, endY - 1, inputMap, map, period))
    val backAgain = Util.bfs(back.path.last, isTarget(targetPos), neighbors(endX, endY - 1, inputMap, map, period))

    //Part 2
    println(backAgain.path.last._2)

  @tailrec
  def buildBlizzardMap(blizzards: Set[(Int, Int, Char)], maxX: Int, maxY: Int, acc: Map[Int, Set[(Int, Int, Char)]], time: Int = 1): (Int, Map[Int, Set[(Int, Int, Char)]]) =
    val next = step(blizzards, maxX, maxY)

    if acc.values.toSet.contains(next) then
      (time, acc)
    else
      buildBlizzardMap(next, maxX, maxY, acc + (time -> next), time + 1)

  def neighbors(maxX: Int, maxY: Int, map: Map[(Int, Int), Char], blizzards: Map[Int, Set[(Int, Int, Char)]], period: Int)(state: ((Int, Int), Int)): List[((Int, Int), Int)] =
    val nextBlizz = blizzards((state._2 + 1) % period)
    val currPos = state._1
    val nextPos = List(currPos, (currPos._1 + 1, currPos._2), (currPos._1 - 1, currPos._2), (currPos._1, currPos._2 + 1), (currPos._1, currPos._2 - 1)).filter(p =>
      if map.contains(p) && map(p) == '#' then
        false
      else if nextBlizz.exists(e => e._1 == p._1 - 1 && e._2 == p._2 - 1) then
        false
      else if !map.contains(p) then
        false
      else
        true
    )

    nextPos.map(p =>
      (p, state._2 + 1)
    )

  def isTarget(targetPos: (Int, Int))(state: ((Int, Int), Int)): Boolean =
    state._1 == targetPos

  def step(blizzards: Set[(Int, Int, Char)], maxX: Int, maxY: Int): Set[(Int, Int, Char)] =
    blizzards.map(e =>
      e._3 match
        case '<' =>
          (e._1, ((e._2 - 1) % maxX + maxX) % maxX, e._3)
        case '>' =>
          (e._1, (e._2 + 1) % maxX, e._3)
        case '^' =>
          (((e._1 - 1) % maxY + maxY) % maxY, e._2, e._3)
        case 'v' =>
          ((e._1 + 1) % maxY, e._2, e._3)
    )