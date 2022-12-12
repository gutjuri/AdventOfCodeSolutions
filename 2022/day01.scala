import scala.io.Source

case class Elve(cals: List[Int])

def readInput: List[Elve] =
    Source.stdin.getLines().foldRight(z = List[Elve](Elve(List[Int]())))((x, i) => 
        if "".equals(x) 
            then { Elve(List[Int]()) :: i}
            else {
                val Elve(s) :: is = i
                Elve(x.toInt :: s) :: is
            }
    )

def mostCals(elves : List[Elve]): Int = elves.map(_.cals.sum).max

def most3Cals(elves: List[Elve]): Int = elves.map(_.cals.sum).sortWith(_ > _).take(3).sum

@main def day1: Unit = 
    val elves = readInput
    println(mostCals(elves))
    println(most3Cals(elves))
