import scala.io.Source
val digits: Map[Char, Long] = Map('2'-> 2l, '1' -> 1l, '0' -> 0l, '-' -> -1l, '=' -> -2l)
val invDigits: Map[Long, Char] = Map(4l -> '2', 3l -> '1', 2l -> '0', 1l -> '-', 0l -> '=')

def snafuToDec(snafu: String): Long =
  snafu.foldLeft(0l)((v, c) => v * 5 + digits(c))

def decToSnafu(n: Long): String =
  var (p, k) = (0l, 1l)
  while p <= n do
    p += 2*k
    k *= 5
  var (res, nx) = ("", n + p)
  while nx != 0 do
    res = invDigits(nx % 5l) + res
    nx /= 5
  return res

def parseInput: Array[String] =
  Source.fromFile("../in25.txt").getLines().toArray

def task1(input: Array[String]): String =
  println(input.map(snafuToDec(_)).sum)
  decToSnafu(input.map(snafuToDec(_)).sum)

@main def hello: Unit = 
  val input = parseInput
  println(task1(input))
  
