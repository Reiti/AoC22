package util

trait Day(day: Int):
  lazy val input: String = Util.loadDay(day)
  lazy val rawInput: String = Util.loadDayKeepWhitespace(day)
  lazy val inputLines: List[String] = input.split("\n").toList
  lazy val inputInts: List[Int] = inputLines.map(_.toInt)
  lazy val inputMap: Map[(Int, Int), Char] = inputLines.indices.flatMap(col => inputLines.head.indices.map(row => {
    (col, row) -> inputLines(col)(row)
  })).toMap

  def solve(): Unit

  def main(args: Array[String]): Unit = solve()



