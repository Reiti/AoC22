import util.{Day, Util}

import scala.annotation.tailrec

case class Sensor(x: Int, y: Int, bx: Int, by: Int):
  val dist: Int = Util.manhattan((x,y), (bx, by))

object Day15 extends Day(15):
  override def solve(): Unit =
    val sensors = inputLines.map { case s"Sensor at x=$x, y=$y: closest beacon is at x=$bx, y=$by" => Sensor(x.toInt, y.toInt, bx.toInt, by.toInt) }

    //Part 1
    println(checkLine(sensors, 2000000, minX(sensors), maxX(sensors)))

    val beacon = sensors.view.flatMap(i => ring(sensors, i)).head

    //Part 2
    println(beacon._1.toLong * 4000000L + beacon._2.toLong)

  def checkLine(sensors: List[Sensor], y: Int, start: Int, stop: Int): Int =
    (start to stop).count(x => !canBeaconExist(sensors, x, y, true))

  def maxX(sensors: List[Sensor]): Int =
    sensors.map(s => s.x + s.dist).max

  def minX(sensors: List[Sensor]): Int =
    sensors.map(s => s.x - s.dist).min

  def ring(sensors: List[Sensor], sensor: Sensor): Option[(Int, Int)] =
    val d = sensor.dist + 1

    (0 to d).view
      .flatMap(v => List((v, Math.abs(d) - Math.abs(v)), (v, -(Math.abs(d) - Math.abs(v))), (-v, Math.abs(d) - Math.abs(v)), (-v, -(Math.abs(d) - Math.abs(v)))))
      .map(c => (sensor.x + c._1, sensor.y + c._2))
      .filter(c => (0 <= c._1 && c._1 <= 4000000) && (0 <= c._2 && c._2 <= 4000000))
      .find(c => canBeaconExist(sensors, c._1, c._2, false))

  @tailrec
  def canBeaconExist(sensors: List[Sensor], x: Int, y: Int, countOwn: Boolean): Boolean = sensors match
    case sensor :: xs =>
      val currDist = Util.manhattan(sensor.x, sensor.y, x, y)

      if sensor.bx == x && sensor.by == y then
        countOwn
      else if currDist <= sensor.dist then
        false
      else
        canBeaconExist(xs, x, y, countOwn)
    case _ => true