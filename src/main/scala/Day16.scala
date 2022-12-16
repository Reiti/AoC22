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
/*
    Util.time {
      println(maxRelease(parse(inputLines)))
    }
    */
    Util.time {
      println(maxReleaseElephant(parse(testInput)))
    }

  @tailrec
  def parse(lines: List[String], acc: Map[String, (Int, List[String])] = Map()): Map[String, (Int, List[String])] = lines match
    case x :: xs =>
      val (v, r, n) = x match {
        case s"Valve $name has flow rate=$rate; tunnels lead to valves $leads" => (name, rate.toInt, leads.split(", ").toList)
        case s"Valve $name has flow rate=$rate; tunnel leads to valve $leads" => (name, rate.toInt, List(leads))
      }
      parse(xs, acc + (v -> (r, n)))
    case _ => acc

  def maxRelease(valves: Map[String, (Int, List[String])]): Int =
    var globalMax = 0
    val t = 30
    lazy val maxReleaseH: ((String, Int, Set[String], Int)) =>  Int = Util.memoize {
      case (curr, currFlow, opened, time) =>
        if time == t then
          if currFlow > globalMax then
            globalMax = currFlow
          currFlow
        else if opened.contains(curr) || valves(curr)._1 == 0 then
          val rem = valves.keys.filter(k => !opened.contains(k)).map(k => valves(k)._1).sum * Math.max(t - time - 1, 0)
          if (currFlow + rem) < globalMax then
            currFlow
          else
            valves(curr)._2.map(n => maxReleaseH(n, currFlow, opened, time + 1)).max
        else
          val rem = valves.keys.filter(k => !opened.contains(k)).map(k => valves(k)._1).sum * Math.max(t - time - 1, 0)
          if (currFlow + rem) < globalMax then
            currFlow
          else
            Math.max(valves(curr)._2.map(n => maxReleaseH(n, currFlow, opened, time + 1)).max, valves(curr)._2.map(n => maxReleaseH(n,
              currFlow + valves(curr)._1 * Math.max(t - time - 1, 0), opened + curr, Math.min(t, time + 2))).max
            )
    }
    maxReleaseH("AA", 0, Set(), 0)

  def maxReleaseElephant(valves: Map[String, (Int, List[String])]): Int =
    var globalMax = 0
    val t = 26
    lazy val maxReleaseElephantH: ((String, String, Int, Set[String], Int)) => Int = Util.memoize {
      case (m, e, currFlow, opened, time) =>
        if time == t then
          if currFlow > globalMax then
            globalMax = currFlow
          currFlow
        else if opened.contains(m) || valves(m)._1 == 0 then
          if opened.contains(e) || valves(e)._1 == 0 then
            val rem = valves.keys.filter(k => !opened.contains(k)).map(k => valves(k)._1).sum * Math.max(t - time - 1, 0)
            if (currFlow + rem) < globalMax then
              currFlow
            else
              valves(m)._2.flatMap(nm => valves(e)._2.map(ne => maxReleaseElephantH(nm, ne, currFlow, opened, time + 1))).max
          else
            0
        else
          0




    }
    maxReleaseElephantH("AA", "AA", 0, Set(), 0)