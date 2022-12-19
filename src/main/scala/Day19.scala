import util.{Day, Util}
import RobotType.*

enum RobotType:
  case ORE, CLAY, OBSIDIAN, GEODE

case class Blueprint(number: Int, ore: (Int, Int, Int), clay: (Int, Int, Int), obsidian: (Int, Int, Int), geode: (Int, Int, Int)):
  def canBuild(o: Int, c: Int, ob: Int, t: RobotType): Boolean = t match
    case ORE => canBuild(o, c, ob, ore)
    case CLAY => canBuild(o, c, ob, clay)
    case OBSIDIAN => canBuild(o, c, ob, obsidian)
    case GEODE => canBuild(o, c, ob, geode)

  def build(o: Int, c: Int, ob: Int, t: RobotType): (Int, Int, Int) = t match
    case ORE => build(o, c, ob, ore)
    case CLAY => build(o, c, ob, clay)
    case OBSIDIAN => build(o, c, ob, obsidian)
    case GEODE => build(o, c, ob, geode)

  private def canBuild(o: Int, c: Int, ob: Int, cost: (Int, Int, Int)): Boolean = o >= cost._1 && c >= cost._2 && ob >= cost._3

  private def build(o: Int, c: Int, ob: Int, cost: (Int, Int, Int)): (Int, Int, Int) = (o - cost._1, c - cost._2, ob - cost._3)

object Day19 extends Day(19):
  override def solve(): Unit =
    val blueprints = inputLines.map(parse)

    //Part 1
    println(blueprints.map(b => b.number * calcGeodes(b, 24)).sum)

    //Part 2
    println(blueprints.take(3).map(b => calcGeodes(b, 32)).product)

  def parse(line: String): Blueprint = line match
    case s"Blueprint $n: Each ore robot costs $oo ore. Each clay robot costs $co ore. Each obsidian robot costs $obo ore and $obc clay. Each geode robot costs $go ore and $gob obsidian." =>
      Blueprint(n.toInt, (oo.toInt, 0, 0), (co.toInt, 0, 0), (obo.toInt, obc.toInt, 0), (go.toInt, 0, gob.toInt))

  def calcGeodes(blueprint: Blueprint, maxTime: Int): Int =
    lazy val calcGeodes: ((Int, Int, Int, Int, Int, Int, Int)) => Int = Util.memoize {
      case (time, _, _, _, _, _, _) if time == maxTime => 0
      case (_, _, _, _, _, _, obsidianBots) if obsidianBots == blueprint.geode._3 => 0
      case (_, _, _, _, _, clayBots, _) if clayBots == blueprint.obsidian._2 => 0
      case (_, _, _, _, oreBots, _, _) if oreBots == (blueprint.geode._1 + blueprint.clay._1) => 0
      case (time, ore, clay, obsidian, oreBots, clayBots, obsidianBots) =>
        if blueprint.canBuild(ore, clay, obsidian, GEODE) then
          (maxTime - time - 1) + calcGeodes(time + 1, ore + oreBots - blueprint.geode._1, clay + clayBots - blueprint.geode._2, obsidian + obsidianBots - blueprint.geode._3, oreBots, clayBots, obsidianBots)
        else
          max(
            calcGeodes(time + 1, ore + oreBots, clay + clayBots, obsidian + obsidianBots, oreBots, clayBots, obsidianBots),
            if blueprint.canBuild(ore, clay, obsidian, ORE) then
              val remaining = blueprint.build(ore, clay, obsidian, ORE)
              calcGeodes(time + 1, remaining._1 + oreBots, remaining._2 + clayBots, remaining._3 + obsidianBots, oreBots + 1, clayBots, obsidianBots)
            else
              0
            ,
            if blueprint.canBuild(ore, clay, obsidian, CLAY) then
              val remaining = blueprint.build(ore, clay, obsidian, CLAY)
              calcGeodes(time + 1, remaining._1 + oreBots, remaining._2 + clayBots, remaining._3 + obsidianBots, oreBots, clayBots + 1, obsidianBots)
            else
              0
            ,
            if blueprint.canBuild(ore, clay, obsidian, OBSIDIAN) then
              val remaining = blueprint.build(ore, clay, obsidian, OBSIDIAN)
              calcGeodes(time + 1, remaining._1 + oreBots, remaining._2 + clayBots, remaining._3 + obsidianBots, oreBots, clayBots, obsidianBots + 1)
            else
              0
          )
    }
    calcGeodes(0, 0, 0, 0, 1, 0, 0)

  def max(l: Int*): Int = l.max