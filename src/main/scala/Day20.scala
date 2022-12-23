import util.Day

import scala.annotation.tailrec

object Day20 extends Day(20):
  override def solve(): Unit =
    val inp = inputInts.map(_.toLong)

    val arr  = inp.toArray.zipWithIndex
    val mixed = mix(inp.zipWithIndex, arr).map(_._1)
    val zero = mixed.indexOf(0L)

    //Part 1
    println(mixed((zero + 1000) % mixed.length) + mixed((zero + 2000) % mixed.length) + mixed((zero + 3000) % mixed.length))

    val decryptionKey = 811589153L
    val inpActual = inp.map(_ * decryptionKey)
    val arrActual = inpActual.toArray.zipWithIndex
    val mixedActual = mix(inpActual.zipWithIndex, arrActual, 10).map(_._1)
    val zeroActual = mixedActual.indexOf(0L)

    //Part 2
    println(mixedActual((zeroActual + 1000) % mixedActual.length) + mixedActual((zeroActual + 2000) % mixedActual.length) + mixedActual((zeroActual + 3000) % mixedActual.length))

  @tailrec
  def mix(order: List[(Long, Int)], sequence: Array[(Long, Int)], times: Int): Array[(Long, Int)] =
    if times == 0 then
      sequence
    else
      mix(order, mix(order, sequence), times - 1)

  @tailrec
  def mix(remaining: List[(Long, Int)], sequence: Array[(Long, Int)]): Array[(Long, Int)] = remaining match
    case x :: xs =>
      if x._1 == 0 then
        mix(xs, sequence)
      else if x._1 < 0 then
        mix(xs, swapLeft(sequence, sequence.indexOf(x), (-1)*x._1 % (sequence.length-1)))
      else
        mix(xs, swapRight(sequence, sequence.indexOf(x), x._1 % (sequence.length-1)))
    case _ => sequence

  @tailrec
  def swapLeft(arr: Array[(Long, Int)], idx: Int, times: Long): Array[(Long, Int)] =
    if times == 0 then
      arr
    else
      val targetIdx = ((idx - 1) % arr.length + arr.length) % arr.length
      val c = arr(idx)
      arr(idx) = arr(targetIdx)
      arr(targetIdx) = c
      swapLeft(arr, targetIdx, times - 1)

  @tailrec
  def swapRight(arr: Array[(Long, Int)], idx: Int, times: Long): Array[(Long, Int)] =
    if times == 0 then
      arr
    else
      val targetIdx = (idx + 1) % arr.length
      val c = arr(idx)
      arr(idx) = arr(targetIdx)
      arr(targetIdx) = c
      swapRight(arr, targetIdx, times - 1)
