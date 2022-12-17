import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class RepIterator[T](elems: List[T]) extends Iterator[T]:
  var pos = 0
  override def hasNext: Boolean = true
  override def next(): T =
    if pos >= elems.size then pos = 0
    pos += 1
    elems(pos - 1)
  def reset = pos = 0

object Shape:
  private val points1 = List[(Long, Long)]((0, 0), (1, 0), (2, 0), (3, 0))
  private val points2 =
    List[(Long, Long)]((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))
  private val points3 =
    List[(Long, Long)]((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))
  private val points4 = List[(Long, Long)]((0, 0), (0, 1), (0, 2), (0, 3))
  private val points5 = List[(Long, Long)]((0, 0), (0, 1), (1, 0), (1, 1))

  val pit = RepIterator[List[(Long, Long)]](
    List(points1, points2, points3, points4, points5)
  )

class Shape(var points: List[(Long, Long)]):
  def this(points: List[(Long, Long)], startPos: (Long, Long)) =
    this(points.map(p => (p(0) + startPos(0), p(1) + startPos(1))))

  def includesPoint(p: (Long, Long)): Boolean =
    points
      .map(tp => tp(0) == p(0) && tp(1) == p(1))
      .foldRight(false)((a, b) => a || b)

  def canMoveDown(others: Set[Shape]): Boolean =
    points.forall(p => {
      others.forall(other => !other.includesPoint(p(0), p(1) - 1))
    }) && points.forall(p => p(1) > 0)
  def moveDown =
    points = points.map(p => (p(0), p(1) - 1))

  def canMoveSideways(dir: Long, others: Set[Shape]): Boolean =
    points.forall(p => p(0) + dir >= 0 && p(0) + dir <= 6) && points.forall(
      p => {
        others.forall(other => !other.includesPoint((p(0) + dir, p(1))))
      }
    )

  def moveSideways(dir: Long) =
    points = points.map(p => (p(0) + dir, p(1)))

def atoT(a: ArrayBuffer[Long]): (Long, Long, Long, Long, Long, Long, Long) =
  (a(0), a(1), a(2), a(3), a(4), a(5), a(6))

class Field():
  private val shapes: Set[Shape] = Set(
    Shape(
      List[(Long, Long)]((0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0))
    )
  )
  val highest: ArrayBuffer[Long] = ArrayBuffer(0, 0, 0, 0, 0, 0, 0)

  def addShape(moveDirs: RepIterator[Char]) =
    val startPos = (2.toLong, highest.max + 4)
    val shape = Shape(Shape.pit.next(), startPos)

    if debug then println(s"adding shape ${shape.points}")

    var canMove = true
    while canMove do
      val moveDir = if moveDirs.next() == '<' then -1 else 1
      if debug then println(s"trying to move $moveDir")
      if shape.canMoveSideways(moveDir, shapes) then
        if debug then println(s"moving shape $moveDir")
        shape.moveSideways(moveDir)
      if shape.canMoveDown(shapes) then
        if debug then println("moving shape down")
        shape.moveDown
      else canMove = false
      if debug then printField(shapes + shape)
      if debug then println()

    shapes.add(shape)
    0.to(6)
      .foreach(x =>
        highest(x) = highest(x)
          .max(shapes.flatMap(s => s.points.filter(_(0) == x).map(_(1))).max)
      )
    // println(highest)
    // println(shape.points)
    shapes.filterInPlace(s => {
      s.points.map(_(1)).max >= highest.min - 10
    })

  def printField(shapes: Set[Shape] = this.shapes) =
    // shapes.foreach(s => println(s.points))
    20.to(0, -1)
      .foreach(y =>
        0.to(6)
          .foreach(x =>
            print(
              if shapes.forall(s => !s.includesPoint((x, y))) then '.'
              else '#'
            )
          )
        println()
      )

def parseInput: RepIterator[Char] =
  val line = Source.fromFile("../in17.txt").getLines().next()
  RepIterator[Char](line.toList)

def debug = false

type Heights = (Long, Long, Long, Long, Long, Long, Long)

val cycles: Map[(Long, Long, Heights), (Long, Long)] = Map()

def calcUntil(round: Long, moveDirs: RepIterator[Char]): Long =
  val field = Field()
  var i = 0
  while i < round do
    val key = (
      moveDirs.pos.toLong,
      Shape.pit.pos.toLong,
      atoT(field.highest.map(x => x - field.highest.min))
    )

    if cycles.contains(key) then
      val (s, highest) = cycles(key)
      val period = i - s
      
      val remainingCycles = Math.floorDiv(round - i, period)
      val remainingRounds = Math.floorMod(round - i, period)
      var hAfterCycles: Long = field.highest.max
      var j = 0L
      while j < remainingRounds do
        field.addShape(moveDirs)
        j += 1
      return (field.highest.max - highest) + highest + (hAfterCycles - highest) * remainingCycles
    else cycles(key) = (i, field.highest.max)
    field.addShape(moveDirs)
    i += 1

  return field.highest.max

@main def hello: Unit =
  val nextDirs = parseInput
  println(calcUntil(2022, nextDirs))
  nextDirs.reset

  val field = Field()
  val goal = 1000000000000L
  nextDirs.reset
  Shape.pit.reset
  cycles.clear()

  println(calcUntil(goal, nextDirs))
