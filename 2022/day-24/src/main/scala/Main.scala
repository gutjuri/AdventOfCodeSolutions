import scala.io.Source
import scala.collection.mutable.PriorityQueue

@main def hello: Unit =
  val (bs, endPoint) = parseInput
  println(endPoint)
  val fieldsz = (endPoint(0), endPoint(1) -1)
  val pq = PriorityQueue[State]()(using Ordering.by(s => -s(1)))
  pq.enqueue(((0, -1), 0))
  val trip1 = bfs(pq, bs, endPoint, Set.empty, fieldsz)

  println(trip1)
  pq.clear()
  pq.enqueue((endPoint, trip1))
  val trip2 = bfs(pq, bs, (0, -1), Set.empty, fieldsz)
  println(trip2)
  pq.clear()
  pq.enqueue(((0, -1), trip2))
  val trip3 = bfs(pq, bs, endPoint, Set.empty, fieldsz)
  println(trip3)

type Point = (Int, Int)
type State = (Point, Int)

def adjPoints(p: Point, startPoint: Point, endPoint: Point, fieldsz: Point): List[Point] =
  val ((px, py), (ex, ey), (sx, sy)) = (p, endPoint, fieldsz)
  List((px, py), (px + 1, py), (px - 1, py), (px, py + 1), (px, py - 1)).filter(
    (x, y) =>
      val t = (x, y) == endPoint 
        || (x, y) ==  startPoint
        || (x >= 0 && x <= sx && y >= 0 && y <= sy)
      //println(s"${(x, y)} $t $fieldsz $endPoint")
      t

  )

def bfs(
    states: PriorityQueue[State],
    bs: List[Int => Point],
    endPoint: Point,
    dp: Set[State],
    fieldsz: Point
): Int =
  val (pos, round) = states.dequeue()
  //println(s"$pos $endPoint $round")
  if pos == endPoint then round
  else if dp.contains((pos, round%(fieldsz(0) * fieldsz(1)))) then bfs(states, bs, endPoint, dp, fieldsz)
  else
    val adj = adjPoints(pos, pos, endPoint, fieldsz)
    //println(adj)
    val occ = occPos(bs, round + 1)
    val nextStates =
      adj.filter(p => !occ.contains(p)).map(npos => (npos, round + 1))
    states.enqueue(nextStates: _*)
    bfs(states, bs, endPoint, dp + ((pos, round%(fieldsz(0) * fieldsz(1)))), fieldsz)

def occPos(bs: List[Int => Point], round: Int): Set[Point] =
  bs.map(_(round)).toSet

def mod(a: Int, b: Int): Int =
  val res = a % b
  if res < 0 then b + res else res

def parseInput: (List[Int => Point], Point) =
  val lines = Source.fromFile("../in24.txt").getLines().toArray
  val (maxX, maxY) = (lines(0).length - 2, lines.length - 2)

  val bs = lines.tail.zipWithIndex
    .flatMap((ln, row) =>
      ln.tail.zipWithIndex
        .filter(t => t(0) != '.' && t(0) != '#')
        .map((c, col) =>
          c match
            case '>' => (n: Int) => ((n + col) % maxX, row)
            case '<' => (n: Int) => (mod(col - n, maxX), row)
            case '^' => (n: Int) => (col, mod(row - n, maxY))
            case 'v' => (n: Int) => (col, (row + n) % maxY)
        )
    )
    .toList
  (bs, (maxX - 1, maxY))
