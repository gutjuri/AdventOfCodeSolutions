import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Queue

type Coords = (Int, Int, Int)

class Cube(exists: Boolean)

def getNeighbourCoords(coords: Coords): Array[Coords] =
  val (x, y, z) = coords
  Array(
    (x + 1, y, z),
    (x - 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  )

def parseInput: Map[Coords, Cube] =
  Source
    .fromFile("../in18.txt")
    .getLines()
    .map(ln =>
      val nrs = ln.split(",").map(_.toInt)
      val coords = (nrs(0), nrs(1), nrs(2))
      (coords, Cube(true))
    )
    .toMap

def countSurfaceArea(coords: Map[Coords, Cube]): Int =
  coords
    .map((coord, cube) =>
      val neighbourCoords = getNeighbourCoords(coord)
      6 - neighbourCoords.filter(coords.contains(_)).length
    )
    .sum

def plus(c: Coords, i: Int): Coords =
  (c(0) + i, c(1) + i, c(2) + i)

def countOpenSurfaceArea(coords: Map[Coords, Cube]): Int =
  val maxB = plus(
    coords.foldRight((0, 0, 0))((x, i) =>
      (x(0)(0).max(i(0)), x(0)(1).max(i(1)), x(0)(2).max(i(2)))
    ),
    1
  )
  val minB = plus(
    coords.foldRight((0, 0, 0))((x, i) =>
      (x(0)(0).min(i(0)), x(0)(1).min(i(1)), x(0)(2).min(i(2)))
    ),
    -1
  )
  val airCubes = Set[Coords]()
  val toVis = Queue(minB)

  while !toVis.isEmpty do
    val next = toVis.dequeue()
    getNeighbourCoords(next)
      .filter(c =>
        !(coords.contains(c) || airCubes.contains(c)) &&
          (maxB(0) >= c(0) && c(0) >= minB(0) && maxB(1) >= c(1) && c(
            1
          ) >= minB(1) && maxB(2) >= c(2) && c(2) >= minB(2))
      )
      .foreach(c => {
        airCubes.add(c)
        toVis.enqueue(c)
      })

  coords
    .map((coord, cube) =>
      val neighbourCoords = getNeighbourCoords(coord)
      6 - neighbourCoords
        .filter(c => coords.contains(c) || !airCubes.contains(c))
        .length
    )
    .sum

@main def hello: Unit =
  val coords = parseInput
  println(countSurfaceArea(coords))
  println(countOpenSurfaceArea(coords))
