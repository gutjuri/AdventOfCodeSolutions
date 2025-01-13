import scala.io.Source
import scala.util.matching.Regex
import javax.lang.model.element.ModuleElement.Directive

sealed trait Instr

case class Forward(amount: Int) extends Instr

case class Left() extends Instr

case class Right() extends Instr

type Dir = (Int, Int)

def mod(a: Int, mod: Int): Int =
  val res = a % mod
  if res < 0 then res + mod else res

def move(
    pos: (Int, Int, Dir),
    instr: Instr,
    field: Map[(Int, Int), Char]
): (Int, Int, Dir) =
  val maxX = field.keySet.map(_(0)).max + 1
  val maxY = field.keySet.map(_(1)).max + 1
  //println(s"$maxX $maxY")

  def getNext(x: Int, y: Int, cdirX: Int, cdirY: Int): (Int, Int) =
    var i = 1
    while !field.contains((mod(x + i * cdirX, maxX), mod(y + i * cdirY, maxY)))
    do i += 1
    (mod(x + i * cdirX, maxX), mod(y + i * cdirY, maxY))

  val (x, y, (dirX, dirY)) = pos
  //println(instr)
  instr match
    case Forward(amount) => {
      var i = 1
      var (cx, cy) = (x, y)
      while i <= amount do
        val (nextX, nextY) = getNext(cx, cy, dirX, dirY)
        //println(s"next: $nextX $nextY")
        if field((nextX, nextY)) == '#' then i = amount + 1
        else
          cx = nextX
          cy = nextY
          i += 1
      (cx, cy, (dirX, dirY))
    }
    case Right()  => (x, y, (-dirY, dirX))
    case Left() => (x, y, (dirY, -dirX))

def splitOnChange(str: String): List[Instr] =
  if str.equals("") then List()
  else if str.charAt(0).isDigit then
    Forward(str.takeWhile(_.isDigit).toInt) :: splitOnChange(
      str.dropWhile(_.isDigit)
    )
  else if str.charAt(0) == 'L' then Left() :: splitOnChange(str.tail)
  else Right() :: splitOnChange(str.tail)

def parseInput: ((Int, Int), Map[(Int, Int), Char], List[Instr]) =
  val lines = Source.fromFile("../in22.txt").getLines().toArray
  val mapping = lines
    .take(lines.length - 2)
    .zipWithIndex
    .flatMap((ln, i) =>
      ln.zipWithIndex.filter((c, _) => c != ' ').map((c, j) => ((j, i), c))
    )
  val instr = splitOnChange(lines.last)
  (mapping.head(0), mapping.toMap, instr)

def facingValue(dir: Dir): Int =
  dir match
    case (1, 0)  => 0
    case (0, 1)  => 1
    case (-1, 0) => 2
    case (0, -1) => 3

def part1(
    start: (Int, Int, Dir),
    field: Map[(Int, Int), Char],
    instr: List[Instr]
): Int =
  val (endX, endY, endDir) = instr.foldLeft(start)((pos, i) =>
    println((pos(0), pos(1), facingValue(pos(2)), i))
    move(pos, i, field)
  )
  println(s"$endX $endY ${facingValue(endDir)}")
  (endY + 1) * 1000 + 4 * (endX + 1) + facingValue(endDir)

def move2(
    pos: (Int, Int, Dir),
    instr: Instr,
    field: Map[(Int, Int), Char]
): (Int, Int, Dir) =
  val maxX = field.keySet.map(_(0)).max + 1
  val maxY = field.keySet.map(_(1)).max + 1
  //println(s"$maxX $maxY")

  def getNext(x: Int, y: Int, cdirX: Int, cdirY: Int): (Int, Int, Dir) =
    var i = 1
    while !field.contains((mod(x + i * cdirX, maxX), mod(y + i * cdirY, maxY)))
    do i += 1
    (mod(x + i * cdirX, maxX), mod(y + i * cdirY, maxY))

  val (x, y, (dirX, dirY)) = pos
  //println(instr)
  instr match
    case Forward(amount) => {
      var i = 1
      var (cx, cy) = (x, y)
      while i <= amount do
        val (nextX, nextY) = getNext(cx, cy, dirX, dirY)
        //println(s"next: $nextX $nextY")
        if field((nextX, nextY)) == '#' then i = amount + 1
        else
          cx = nextX
          cy = nextY
          i += 1
      (cx, cy, (dirX, dirY))
    }
    case Right()  => (x, y, (-dirY, dirX))
    case Left() => (x, y, (dirY, -dirX))

@main def hello: Unit =
  val ((startX, startY), field, instr) = parseInput
  //println(instr)
  println(part1((startX, startY, (1, 0)), field, instr))
