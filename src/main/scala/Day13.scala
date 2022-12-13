import util.Day

import scala.annotation.tailrec
import scala.util

object Day13 extends Day(13):
  override def solve(): Unit =
    //Part 1
    println(inputLines.grouped(3).map(l => (l.head, l(1))).map(compare).zipWithIndex.filter(_._1.get == true).map(_._2 + 1).sum)

    val sorted = inputLines.filter(_.nonEmpty).appended("[[2]]").appended("[[6]]").sortWith((a, b) => compare(a, b).get)
    val idx1 = sorted.indexOf("[[2]]") + 1
    val idx2 = sorted.indexOf("[[6]]") + 1

    //Part 2
    println(idx1 * idx2)

  def compare(l: String, r: String): Option[Boolean] =
    if l.startsWith("[") && r.startsWith("[") then
      val nl = split(l.tail.init)
      val nr = split(r.tail.init)
      comp(nl, nr)
    else if l.startsWith("[") then
      val nl = split(l.tail.init)
      val nr = List(r)
      comp(nl, nr)
    else if r.startsWith("[") then
      val nl = List(l)
      val nr = split(r.tail.init)
      comp(nl, nr)
    else
      if l.toInt == r.toInt then
        None
      else
        Some(l.toInt < r.toInt)

  @tailrec
  def comp(l: List[String], r: List[String]): Option[Boolean] =
    if l.isEmpty && r.isEmpty then
      None
    else if l.isEmpty then
      Some(true)
    else if r.isEmpty then
      Some(false)
    else
      val c = compare(l.head, r.head)
      if c.nonEmpty then
        Some(c.get)
      else
        comp(l.tail, r.tail)

  @tailrec
  def split(arr: String, acc: List[String] = List()): List[String] =
    if arr.isEmpty then
      acc.map(_.reverse.dropWhile(_ == ',').reverse)
    else if arr.startsWith("[") then
      val count = skipArray(arr.drop(1), 1)
      val n = arr.drop(count + 2)
      val v = arr.take(count + 1)
      split(n, acc.appended(v))
    else
      val n = arr.dropWhile(_ != ',').drop(1)
      val v = arr.takeWhile(_ != ',')
      split(n, acc.appended(v))

  @tailrec
  def skipArray(arr: String, l: Int = 0, r: Int = 0, c: Int = 0): Int =
    if arr.isEmpty || l == r then
      c
    else if arr(c).equals('[') then
      skipArray(arr, l + 1, r, c + 1)
    else if arr(c).equals(']') then
      skipArray(arr, l, r + 1, c + 1)
    else
      skipArray(arr, l, r, c + 1)