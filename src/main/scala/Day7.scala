import util.Util

import scala.annotation.tailrec

object Day7:
  sealed trait Node
  case class Dir(name: String) extends Node
  case class File(name: String, size: Int) extends Node

  def main(args: Array[String]): Unit =
    val dirs = parse("", Util.loadDayLines(7), Map())

    val sizes = dirs.keys.map(d => (d.name, size(dirs, d))).toMap

    //Part 1
    println(sizes.values.filter(_ <= 100000).sum)

    val target = 30000000 - (70000000 - sizes("/"))

    //Part 2
    println(sizes.values.filter(_ >= target).min)

  @tailrec
  def parse(prefix: String, instructions: List[String], directories: Map[Dir, List[Node]]): Map[Dir, List[Node]] = instructions match
    case s"$$ cd $name" :: xs => name match
      case "/" => parse("", xs, directories)
      case ".." => parse(prefix.reverse.dropWhile(_ != '/').reverse.init, xs, directories)
      case _ => parse(prefix + "/" + name, xs, directories)
    case s"$$ ls" :: xs => parse(prefix, xs, directories)
    case s"dir $name" :: xs => parse(prefix, xs, directories + (Dir(prefix) -> directories.getOrElse(Dir(prefix), List()).appended(Dir(prefix + "/" + name))))
    case s"$size $name" :: xs => parse(prefix, xs, directories + (Dir(prefix) -> directories.getOrElse(Dir(prefix), List()).appended(File(name, size.toInt))))
    case _ => (directories - Dir("")) + (Dir("/") -> directories(Dir(""))) //Rename "" to "/" for aesthetic reasons

  def size(dirs: Map[Dir, List[Node]], node: Node): Int = node match
    case File(name, size) => size
    case d: Dir => dirs.getOrElse(d, List()).map(n => size(dirs, n)).sum