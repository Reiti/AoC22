import util.{Day, Util}

import scala.annotation.tailrec

object Day16 extends Day(16):
  override def solve(): Unit =
    val (rates, edges) = parse(inputLines)
    val graph = simplify(rates, edges.groupMap(_._1)(_._2))

    //Part 1
    println(maxFlow(rates, graph))

    //Part 2
    println(maxFlowE(rates, graph))

  def simplify(rates: Map[String, Int], edges: Map[String, Set[String]]): Map[String, Set[(String, Int)]] =
    val relevantStarts = rates.filter(k => k._1 == "AA" || k._2 != 0)
    val relevantEnds = relevantStarts.filter(k => k._1 != "AA")

    relevantStarts.map(s =>
      (s._1, relevantEnds.filter(_._1 != s._1).map(e => (e._1, Util.bfs(s._1, e._1, edges.apply.andThen(_.toList)).distance)).toSet)
    )

  def parse(l: List[String]): (Map[String, Int], Set[(String, String)]) = l.foldLeft(Map[String, Int](), Set[(String, String)]())((acc, x) =>
    x match {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $n" =>
        val edges = n.split(", ").flatMap(e => List((name, e), (e, name))).toSet
        (acc._1 + (name -> rate.toInt), acc._2 ++ edges)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $n" =>
        (acc._1 + (name -> rate.toInt), acc._2 ++ Set((name, n), (n, name)))
    }
  )

  def maxFlow(rates: Map[String, Int], graph: Map[String, Set[(String, Int)]]): Int =
    lazy val maxFlow: ((String, Int, Set[String])) => Int = Util.memoize {
      case (_, time, _) if time > 30 => 0
      case (currentNode, time, opened) =>
        if opened.contains(currentNode) then
          graph(currentNode).map(n =>
            maxFlow(n._1, time + n._2, opened)
          ).max
        else
          Math.max(
            rates(currentNode) * Math.max(30 - time - 1, 0) + graph(currentNode).map(n =>
              maxFlow(n._1, time + n._2 + 1, opened + currentNode)
            ).max,
            graph(currentNode).map(n =>
              maxFlow(n._1, time + n._2, opened)
            ).max,
          )
    }
    maxFlow("AA", 0, Set())

  def maxFlowE(rates: Map[String, Int], graph: Map[String, Set[(String, Int)]]): Int =
    lazy val maxFlowE: ((String, Int, String, Int, Set[String])) => Int  = Util.memoize {
      case (_, _, _, _, unopened) if unopened.isEmpty => 0
      case (_, nextM, _, nextE, _) if Math.min(nextM, nextE) > 26 => 0
      case (_, nextM, _, nextE, unopened) if (26 - Math.min(nextM, nextE)) < unopened.size - (if nextM == nextE then 2 else 1) => 0
      case (m, _, e, _, unopened) if !unopened.contains(m) || !unopened.contains(e) => 0
      case (m, nextM, e, nextE, _) if m == e && nextM > nextE => 0
      case (m, nextM, e, nextE, unopened) =>
        if nextM <= nextE then
            rates(m) * Math.max(26 - nextM - 1, 0) + (unopened - m).toList.sortBy(k => rates(k)).reverse.map(n =>
              val d = graph(m).find(_._1 == n).get._2
              maxFlowE(n, nextM + d + 1, e, nextE, unopened - m)
            ).maxOption.getOrElse(0)
        else if nextE <= nextM then
            rates(e) * Math.max(26 - nextE - 1, 0) + (unopened - e).toList.sortBy(k => rates(k)).reverse.map(n =>
              val d = graph(e).find(_._1 == n).get._2
              maxFlowE(m, nextM, n, nextE + d + 1, unopened - e)
            ).maxOption.getOrElse(0)
        else
          maxFlowE(m ,nextM, e, nextE, unopened)
    }
    val next = graph("AA").toList.sortBy(_._2)

    next.flatMap(m => next.dropWhile(_._1 != m._1).map(e => (m ,e))).filter(k => k._1._1 != k._2._1).foldLeft(0)((acc, x) =>
      Math.max(maxFlowE(x._1._1, x._1._2, x._2._1, x._2._2, rates.filter(_._2 != 0).keys.toSet), acc)
    )