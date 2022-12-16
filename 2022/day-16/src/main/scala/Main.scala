import scala.io.Source

case class Valve(rate: Int, leadTo: Array[String], visited: Boolean)

def rex =
  """^Valve ([A-Z]{2}) has flow rate=(\d*); tunnels? leads? to valves? ([A-Z, ]*)$""".r

def getDistances(
    pos: String,
    steps: Int,
    vis: Set[String],
    valves: Map[String, Valve],
    toVis: List[(String, Int)],
    ret: Map[String, Int]
): Map[String, Int] =
  val cValve = valves(pos)
  val newVis = vis + pos
  val newToVis =
    toVis ++ cValve.leadTo.filter(v => !vis.contains(v)).map((_, steps + 1))
  val newRet = ret.updated(pos, steps)
  if newToVis.isEmpty
  then newRet
  else
    getDistances(
      newToVis.head(0),
      newToVis.head(1),
      newVis,
      valves,
      newToVis.tail,
      newRet
    )


def parseInput: (Map[String, Valve], Map[String, Map[String, Int]]) =
  def parseValves(str: String) = str.split(", ")

  val valves: Map[String, Valve] = Source
    .fromFile("../in16.txt")
    .getLines()
    .map(line =>
      line match
        case rex(name, rate, valves) =>
          (name, Valve(rate.toInt, parseValves(valves), false))
    )
    .toMap
  val dists = valves
    .map((name, v) =>
      (name, getDistances(name, 0, Set.empty, valves, List.empty, Map.empty))
    )
    .toMap
  (valves, dists)

def search(
    pos: String,
    cVal: Int,
    cTime: Int,
    valves: Map[String, Valve],
    dists: Map[String, Map[String, Int]],
    path: Set[String]
): List[(Int, Set[String])] =
  if cTime == 30 then return List((cVal, path))

  val cValve = valves(pos)

  val nextVals = {
    for (nextName: String, nextValve: Valve) <- valves.filter((_, v) =>
        !v.visited && v.rate != 0
      )
    yield {
      val potFlow =
        nextValve.rate * (30 - dists(pos)(nextName) - cTime - 1)

      if potFlow > 0
      then
        search(
          nextName,
          cVal + potFlow,
          cTime + dists(pos)(nextName) + 1,
          valves.updated(nextName, nextValve.copy(visited = true)),
          dists,
          path + nextName
        )
      else List((cVal, path))
    }
  }
  if nextVals.isEmpty then List((cVal, path)) else (cVal, path) :: nextVals.flatten.toList

def getNonOverlapping(sols: List[(Int, Set[String])], valves: Map[String, Valve], dists: Map[String, Map[String, Int]]): Int =
  var i = 0
  sols.map(sol => sol(0) + {
    if sol(0) < 1000 
      then
        0
      else
        val newValves = valves.map((n, v) => if sol(1).contains(n) then (n, v.copy(rate=0)) else (n, v))
        val sols3 = search("AA", 0, 4, newValves, dists, Set.empty)
        sols3.map(_(0)).max
  }).max

@main def day16 =
  val (valves, dists) = parseInput
  val sols = search("AA", 0, 0, valves, dists, Set.empty)
  println(sols.maxBy(_(0))(0))
  val sols2 = search("AA", 0, 4, valves, dists, Set.empty)
  println(getNonOverlapping(sols2, valves, dists))

