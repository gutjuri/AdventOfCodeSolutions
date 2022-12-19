import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable
import scala.compiletime.ops.int
import scala.collection.parallel.CollectionConverters._

val rex =
  """^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d)+ ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$""".r

enum Robot(val produces: Resource):
  case OreRobot extends Robot(Resource.Ore)
  case ClayRobot extends Robot(Resource.Clay)
  case ObsidianRobot extends Robot(Resource.Obsidian)
  case GeodeRobot extends Robot(Resource.Geode)

enum Resource:
  case Ore, Clay, Obsidian, Geode

case class Blueprint(nr: Int, costs: Map[Robot, Map[Resource, Int]])

def parseInput: Array[Blueprint] =
  Source
    .fromFile("../in19.txt")
    .getLines()
    .map(line =>
      line match
        case rex(bpnr, oo, co, obo, obc, go, gobs) =>
          Blueprint(
            bpnr.toInt,
            Map(
              Robot.OreRobot -> Map(Resource.Ore -> oo.toInt),
              Robot.ClayRobot -> Map(Resource.Ore -> co.toInt),
              Robot.ObsidianRobot -> Map(
                Resource.Ore -> obo.toInt,
                Resource.Clay -> obc.toInt
              ),
              Robot.GeodeRobot -> Map(
                Resource.Ore -> go.toInt,
                Resource.Obsidian -> gobs.toInt
              )
            )
          )
    )
    .toArray

def canBuild(bp: Blueprint, r: Robot, rescs: Map[Resource, Int]): Boolean =
  bp.costs(r).forall((res, cost) => rescs(res) >= cost)

def build(
    bp: Blueprint,
    r: Robot,
    rescs: Map[Resource, Int],
    robots: Map[Robot, Int]
): (Map[Resource, Int], Map[Robot, Int]) =
  val newRes = bp
    .costs(r)
    .foldRight(rescs)((c, remainingRes) =>
      remainingRes.updated(c(0), remainingRes(c(0)) - c(1))
    )
  val newRobots = robots.updated(r, robots(r) + 1)
  (newRes, newRobots)

def updateResources(
    robots: Map[Robot, Int],
    rescs: Map[Resource, Int]
): Map[Resource, Int] =
  robots.foldRight(rescs)((crob, newRes) =>
    newRes.updated(crob(0).produces, newRes(crob(0).produces) + crob(1))
  )

def getMaxGeodes(bp: Blueprint, maxRounds: Int): Int =
  val robots = Map[Robot, Int](
    Robot.OreRobot -> 1,
    Robot.ClayRobot -> 0,
    Robot.ObsidianRobot -> 0,
    Robot.GeodeRobot -> 0
  )
  val rescs = Map[Resource, Int](
    Resource.Ore -> 0,
    Resource.Clay -> 0,
    Resource.Obsidian -> 0,
    Resource.Geode -> 0
  )

  val dp = mutable.Map[(Map[Robot, Int], Map[Resource, Int], Int), Int]()

  def isNeeded(
      robot: Robot,
      robots: Map[Robot, Int],
      rescs: Map[Resource, Int],
      round: Int
  ): Boolean =
    if robot == Robot.GeodeRobot then return true
    val nrRobots = robots(robot)
    val maximumUsage = bp.costs.foldRight(0)((rm, sum) =>
      rm(1).getOrElse(robot.produces, 0).max(sum)
    ) * (maxRounds - round)
    nrRobots * (maxRounds - round) + rescs(robot.produces) < maximumUsage

  var best = 0
  def rec(
      robots: Map[Robot, Int],
      rescs: Map[Resource, Int],
      round: Int
  ): (Map[Robot, Int], Map[Resource, Int], Int) =
    if round == maxRounds then return (robots, rescs, rescs(Resource.Geode))
    if dp.contains((robots, rescs, round)) then
      return (robots, rescs, dp((robots, rescs, round)))
    val timeLeft = maxRounds - round
    if (timeLeft * timeLeft + timeLeft) / 2 + robots(
        Robot.GeodeRobot
      ) * timeLeft + rescs(Resource.Geode) < best
    then return (Map.empty, Map.empty, 0)

    val maxWhenBuild = Robot.values
      .filter(isNeeded(_, robots, rescs, round))
      .map(rt =>
        if canBuild(bp, rt, rescs) then
          val (newRescsx, newRobots) = build(bp, rt, rescs, robots)
          val newRescs = updateResources(robots, newRescsx)
          rec(newRobots, newRescs, round + 1)
        else (Map.empty, Map.empty, 0)
      )
      .maxBy(_(2))
    val newRescs = updateResources(robots, rescs)
    val maxWoBuild = rec(robots, newRescs, round + 1)

    val maxTuple = Array(maxWoBuild, maxWhenBuild).maxBy(_(2))
    dp((robots, rescs, round)) = maxTuple(2)
    if maxTuple(2) > best then best = maxTuple(2)
    return maxTuple

  val (bestRobots, bestRec, bestGeode) = rec(robots, rescs, 0)
  // println(bestRobots)
  // println(bestRec)
  // println(dp)
  return bestGeode

@main def hello: Unit =
  val bps = parseInput
  val res1 = bps.par.map(bp => getMaxGeodes(bp, 24) * bp.nr).sum
  println(res1)
  val res2 = bps.take(3).par.map(getMaxGeodes(_, 32)).product
  println(res2)
