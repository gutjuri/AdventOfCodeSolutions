import scala.io.Source
import scala.collection.mutable.ListBuffer

sealed trait Data

case class DataList(data: List[Data]) extends Data

case class DataInt(int: Int) extends Data

case class DataPair(left: Data, right: Data)

def parseInput: Array[DataPair] =
  val pairs = Source.stdin.getLines().mkString("\n").split("\n\n")
  pairs.map(p => {
    val leftright = p.split("\n").map(parseLine)
    DataPair(leftright(0), leftright(1))
  })

def splitSublistAware(line: String): List[String] =
  var levels = 0
  var splitOn = ListBuffer[Int](-1)
  for ((x, i) <- line.view.zipWithIndex) {
    x match
      case '['                => levels += 1
      case ']'                => levels -= 1
      case ',' if levels == 0 => splitOn += i
      case _                  => ()
  }
  splitOn += line.length()
  val subs = splitOn
    .zip(splitOn.tail)
    .map { case (a, b) => line.substring(a + 1, b) }
    .toList
  // subs.foreach(println(_))
  subs

def parseLine(ln: String): Data =
  if ln.equals("") then DataList(List[Data]())
  else if ln.startsWith("[")
  then {
    val elems =
      splitSublistAware(ln.substring(1, ln.length() - 1)).map(parseLine(_))
    DataList(elems)
  } else DataInt(ln.toInt)

def compare(left: Data, right: Data): Int =
  (left, right) match
    case (DataInt(i), DataInt(j)) => i.compareTo(j)
    case (DataList(li), DataList(lj)) =>
      li.zip(lj)
        .map(compare)
        .find(_ != 0)
        .getOrElse(li.length.compareTo(lj.length))
    case (i: DataInt, j: DataList) => compare(DataList(List[Data](i)), j)
    case (i: DataList, j: DataInt) => compare(i, DataList(List[Data](j)))

@main def day13: Unit =
  val inp = parseInput
  val sol1 = inp
    .map(dp => compare(dp.left, dp.right))
    .zipWithIndex
    .filter(_(0) == -1)
    .map(_(1) + 1)
    .sum
  println(sol1)
  val divPacket1 = DataList(List[Data](DataList(List[Data](DataInt(2)))))
  val divPacket2 = DataList(List[Data](DataList(List[Data](DataInt(6)))))
  val sorted = inp
    .flatMap(p => Array[Data](p.left, p.right))
    .concat(Array[Data](divPacket1, divPacket2))
    .sorted(using (x, y) => compare(x, y))
  val index1 = sorted.indexOf(divPacket1)
  val index2 = sorted.indexOf(divPacket2)
  println((index1 + 1) * (index2 + 1))
