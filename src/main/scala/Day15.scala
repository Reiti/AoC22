import util.Day

import scala.annotation.tailrec

case class Sensor(x: Int, y: Int, bx: Int, by: Int)

object Day15 extends Day(15):
  override def solve(): Unit =
    val sensors = inputLines.map { case s"Sensor at x=$x, y=$y: closest beacon is at x=$bx, y=$by" => Sensor(x.toInt, y.toInt, bx.toInt, by.toInt) }

    //Part 1
    println(checkLine(sensors, 2000000, minX(sensors), maxX(sensors)))

    val beacon = sensors.flatMap(i => ring(sensors, i)).head

    //Part 2
    println(beacon._1.toLong * 4000000L + beacon._2.toLong)

  def checkLine(sensors: List[Sensor], y: Int, start: Int, stop: Int): Int =
    (start to stop).map(x => canBeaconExist(sensors, x, y, true)).count(_ == false)

  def maxX(sensors: List[Sensor]): Int =
    sensors.map(s => {
      val dist = distance(s.x, s.y, s.bx, s.by)
      s.x + dist + 100
    }).max

  def minX(sensors: List[Sensor]): Int =
    sensors.map(s => {
      val dist = distance(s.x, s.y, s.bx, s.by)
      s.x - dist - 100
    }).min

  def ring(sensors: List[Sensor], sensor: Sensor): Option[(Int, Int)] =
    val d = distance(sensor.x, sensor.y, sensor.bx, sensor.by) + 1

    (-d to d).flatMap(v => List((v, Math.abs(d) - Math.abs(v)), (v, -(Math.abs(d) - Math.abs(v))))).map(c => (sensor.x + c._1, sensor.y + c._2)).filter(c => (0 <= c._1 && c._1 <= 4000000) && (0 <= c._2 && c._2 <= 4000000)).find(c => canBeaconExist(sensors, c._1, c._2, false))
  /*
    (sensor.x - d to sensor.x + d).flatMap(i => (sensor.y -d to sensor.y + d).map(j => (i, j))).filter(c => distance(c._1, c._2, sensor.x, sensor.y) == d).toSet
  */

  @tailrec
  def canBeaconExist(sensors: List[Sensor], x: Int, y: Int, countOwn: Boolean): Boolean = sensors match
    case sensor :: xs =>
      val emptyDist = distance(sensor.x, sensor.y, sensor.bx, sensor.by)
      val currDist = distance(sensor.x, sensor.y, x, y)

      if sensor.bx == x && sensor.by == y then
        countOwn
      else if currDist <= emptyDist then
        false
      else
        canBeaconExist(xs, x, y, countOwn)
    case _ => true

  def distance(x1: Int, y1: Int, x2: Int, y2: Int): Int = Math.abs(x1 - x2) + Math.abs(y1 - y2)