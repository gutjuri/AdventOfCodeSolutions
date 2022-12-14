import scala.io.Source
    
def pointsBetween(c1: List[Int], c2: List[Int]): IndexedSeq[(Int, Int)] = 
    val minX = c1(0).min(c2(0))
    val maxX = c1(0).max(c2(0))
    val minY = c1(1).min(c2(1))
    val maxY = c1(1).max(c2(1))
    for (x <- minX to maxX; y <- minY to maxY) yield (x, y)

def parseInput : Map[(Int, Int), Char] = 
    val coords = Source.stdin.getLines().flatMap(line => 
        val coordsLine = line.split(" -> ").toList
        val coordsParsed = coordsLine.map(_.split(",").map(_.toInt).toList)
        val coords = coordsParsed.zip(coordsParsed.tail).flatMap(pointsBetween)
        //coordsParsed.zip(coordsParsed.tail).foreach(c => println(s"${c(0)} ${c(1)}"))
        coords
        )
    
    Map.from(coords.map(x => (x, '#')))

def pourSand(minY: Int, cave: Map[(Int, Int), Char]): (Map[(Int, Int), Char], Boolean)=
    var (sX, sY) = (500, 0)
    while (true)
        //println(s"${sX} ${sY} ${minY}")
        if sY > minY then return (cave, false)
        else if !cave.contains((sX, sY+1)) then sY += 1
        else if !cave.contains((sX-1, sY+1)) then { sX -=1; sY +=1 }
        else if !cave.contains((sX+1, sY+1)) then { sX +=1; sY +=1 }
        else return (cave + ((sX, sY) -> 'o'), true)
    (null, false)


def task1(cave: Map[(Int, Int), Char]): Int =
    val minY = cave.keySet.map(_(1)).max
    var i = 0
    var caveX = cave
    var spaceLeft = true
    while (spaceLeft)
        i+=1
        val c = pourSand(minY, caveX)
        caveX = c(0)
        spaceLeft = c(1)
    i - 1

def task2(cave: Map[(Int, Int), Char]): Int =
    val floorLevel = cave.keySet.map(_(1)).max + 2
    var i = 0
    var caveX : Map[(Int, Int), Char] = cave ++ { for x <- (-floorLevel - 10).to(floorLevel + 10) yield ((x+500, floorLevel), '#')}
    while (!caveX.contains((500, 0)))
        i+=1
        val c = pourSand(floorLevel-1, caveX)
        caveX = c(0)
    i

@main def day14 = 
    val cave = parseInput
    println(task1(cave))
    println(task2(cave))