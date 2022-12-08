import util.Util

import scala.annotation.tailrec

object Day8:
  def main(args: Array[String]): Unit =
    val input = Util.loadDayMap(8).map(e => (e._1, e._2.asDigit))

    val vis = input.map(t => Util.vonNeumannNeighborhood.map(d => visible(t._1, t._2, input, d)))

    //Part 1
    println(vis.map(_.map(_._1).reduce(_ || _)).count(_ == true))

    //Part 2
    println(vis.map(_.map(_._2).product).max)

  def visible(pos: (Int, Int), height: Int,  map: Map[(Int, Int), Int], direction: (Int, Int)): (Boolean, Int) =
    @tailrec
    def visibleH(curr: (Int, Int)): (Boolean, Int) = map.get(curr) match
      case Some(h) =>
        if h < height then
          visibleH((curr._1 + direction._1, curr._2 + direction._2))
        else
          (false, Util.manhattan(pos, curr))
      case None => (true, Util.manhattan(pos, curr) - 1)
    visibleH((pos._1 + direction._1, pos._2 + direction._2))