import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.Set
import scala.collection.mutable.BitSet

case class Point(x: Int, y: Int):
    def dist(other: Point): Int =
        (this.x - other.x).abs + (this.y - other.y).abs

val numberRex = """^Sensor at x=(-?\d*), y=(-?\d*): closest beacon is at x=(-?\d*), y=(-?\d*)""".r

def parseInput: Array[(Point, Point)] = 
    Source.stdin.getLines().map(line =>
        line match
            case numberRex(x1, y1, x2, y2) => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    ).toArray

def cannotInLine(line: Int, info: (Point, Point)): Set[Int] =
    val (sensor, beacon) = info
    val distToBeacon = sensor.dist(beacon)
    val distToLine = (sensor.y - line).abs
    if distToLine > distToBeacon
        then Set.empty
        else 
            val (minX, maxX) = (sensor.x - (distToBeacon - distToLine), sensor.x + (distToBeacon - distToLine))
            Set.from(minX.to(maxX))

def posCannotInLine(line: Int, info: Array[(Point, Point)]): Set[Int] =
    val beaconPos = info.filter(_(1).y == line).map(_(1).y).toSet
    val pos = info.map(cannotInLine(line, _)).foldRight(Set.empty : Set[Int])((a, b) => a union b)
    pos diff beaconPos

def nCannotInLine(line: Int, info: Array[(Point, Point)]): Int =
    posCannotInLine(line, info).size

def canPoint(p: Point, info: Array[(Point, Point)]): Boolean =
    info.forall(r => r(0).dist(p) > r(0).dist(r(1)))

def getBoundary(minCoord: Int, maxCoord: Int, info: (Point, Point)): Array[Point] =
    val (sensor, beacon) = info
    val dist = sensor.dist(beacon)
    val boundary = { for (x <- 0.to(dist+1)) 
        yield Array(Point(sensor.x + x, sensor.y + dist + 1 - x)
                ,Point(sensor.x + dist + 1 - x, sensor.y - x)
                ,Point(sensor.x - x, sensor.y - dist - 1 + x)
                ,Point(sensor.x - dist - 1 + x, sensor.y + x)).filter(p => p.x >= minCoord && p.x <= maxCoord && p.y >= minCoord && p.y <= maxCoord)
    }.flatten.toArray
    boundary


def getPossibleLocation(minCoord: Int, maxCoord: Int, info: Array[(Point, Point)]): Point =
    info.flatMap(getBoundary(minCoord, maxCoord, _)).find(canPoint(_, info)).get

@main def day15 =
    val info = parseInput
    println(nCannotInLine(2000000, info))
    val (minCoord, maxCoord) = (1, 4000000)
    val Point(x, y) = getPossibleLocation(minCoord, maxCoord, info)
    println(x.toLong * 4000000.toLong + y.toLong)

