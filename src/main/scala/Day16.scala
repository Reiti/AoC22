import util.{Day, Util}

import scala.annotation.tailrec

object Day16 extends Day(16):
  override def solve(): Unit =
    val testInput = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                      |Valve HH has flow rate=22; tunnel leads to valve GG
                      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin.split("\n").map(_.strip).toList

    val (rates, edges) = parse(testInput)
    val graph = simplify(rates, edges.groupMap(_._1)(_._2))

    //println(maxFlow(rates, graph))

    Util.time {
      println(maxFlowE(rates, graph))
    }


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
    val numberWorkingValves = rates.count(_._2 != 0)
    lazy val maxFlowE: ((String, Int, String, Int, Int, Set[String])) => Int  = Util.memoize {
      case (_, _, _, _, _, opened) if opened.size == numberWorkingValves => 0
      case (_, _, _, _, time, _) if time > 26 => 0
      case (_, _, _, _, time, opened) if (numberWorkingValves - opened.size) > (26 - time) => 0
      case (m, nextM, e, nextE, time, opened) =>
        if time == nextM then
          if opened.contains(m) then
            graph(m).map(n =>
              maxFlowE(n._1, time + n._2, e, nextE, Math.min(time + n._2, nextE), opened)
            ).max
          else
            Math.max(
              rates(m) * Math.max(26 - time - 1, 0) + graph(m).map(n =>
                maxFlowE(n._1, time + n._2 + 1, e, nextE, time, opened + m)
              ).max,
              graph(m).map(n =>
                maxFlowE(n._1, time + n._2, e, nextE, Math.min(time + n._2, nextE), opened)
              ).max
            )
        else if time == nextE then
          if opened.contains(e) then
            graph(e).map(n =>
              maxFlowE(m, nextM, n._1, time + n._2, Math.min(time + n._2, nextM), opened)
            ).max
          else
            Math.max(
              rates(e) * Math.max(26 - time - 1, 0) + graph(e).map(n =>
                maxFlowE(m, nextM, n._1, time + n._2 + 1, time, opened + e)
              ).max,
              graph(e).map(n =>
                maxFlowE(m, nextM, n._1, time + n._2, Math.min(time + n._2, nextM), opened)
              ).max
            )
        else
          maxFlowE(m ,nextM, e, nextE, Math.min(nextM, nextE), opened)
    }
    maxFlowE("AA", 0, "AA", 0, 0, Set())