import util.Util.bfs
import util.{Day, Util}

object Day12 extends Day(12):
  override def solve(): Unit =
    val start = inputMap.find(_._2 == 'S').get._1

    //Part 1
    println(bfs(start, inputMap(_) == 'E', neighbors(inputMap)).distance)

    val starts = inputMap.filter(e => height(e._2) == 'a').keys

    //Part 2
    println(starts.map(s => bfs(s, inputMap(_) == 'E', neighbors(inputMap)).distance).min)

  def neighbors(graph: Map[(Int, Int), Char])(node: (Int, Int)): List[(Int, Int)] =
    Util.vonNeumannNeighborhood.map(e => (e._1 + node._1, e._2 + node._2)).filter(e => graph.contains(e)).filter(e =>
      val curr = graph(node)
      val next = graph(e)
      height(next) <= (height(curr) + 1)).toList

  def height(c: Char): Char = if c == 'S' then 'a' else if c == 'E' then 'z' else c