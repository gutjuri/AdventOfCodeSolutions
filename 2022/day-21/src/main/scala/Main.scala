import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable

sealed trait Answer

def copy(x: Answer, newOp: String): Answer =
  x match
    case Op(par1, par2, _) => Op(par1, par2, newOp)
    case x                 => x

case class Num(num: Long) extends Answer

case class Op(par1: String, par2: String, func: String) extends Answer

case class Input() extends Answer

val numRex = """^(\d*)$""".r

def parseOp(symbol: String): (Long, Long) => Long =
  symbol match
    case "+" => (a, b) => a + b
    case "-" => (a, b) => a - b
    case "/" => (a, b) => a / b
    case "*" => (a, b) => a * b

def parseOpInverse(symbol: String): (Long, Long) => Long =
  symbol match
    case "+" => parseOp("-")
    case "-" => parseOp("+")
    case "/" => parseOp("*")
    case "*" => parseOp("/")

def parseInput: Map[String, Answer] =
  Source
    .fromFile("../in21.txt")
    .getLines()
    .map(ln =>
      val symbols = ln.split(": ")
      val name = symbols(0)
      val ans = symbols(1)
      ans match
        case numRex(n) => (name, Num(n.toLong))
        case somestr => {
          val spl = somestr.split(" ")
          (name, Op(spl(0), spl(2), spl(1)))
        }
    )
    .toMap

def eval(
    monkeys: Map[String, Answer],
    name: String,
    dp: mutable.Map[String, Long]
): Long =
  if dp.contains(name) then dp(name)
  else
    val ans = monkeys(name) match
      case Input() => { println("error"); 0 }
      case Num(n)  => n
      case Op(par1, par2, func) =>
        parseOp(func)(eval(monkeys, par1, dp), eval(monkeys, par2, dp))
    dp(name) = ans
    ans

def subtreeContainsInput(monkeys: Map[String, Answer], name: String): Boolean =
  monkeys(name) match
    case Input() => true
    case Num(_)  => false
    case Op(par1, par2, _) =>
      subtreeContainsInput(monkeys, par1) || subtreeContainsInput(monkeys, par2)

def shouldHave(
    monkeys: Map[String, Answer],
    name: String,
    value: Long
): Long =
  monkeys(name) match
    case Input()  => value
    case Num(num) => { println(s"error $name"); num }
    case Op(par1, par2, func) => {
      if subtreeContainsInput(monkeys, par1) then
        // v = par1 op par2 => par1 = v iop par2
        shouldHave(
          monkeys,
          par1,
          parseOpInverse(func)(value, eval(monkeys, par2, mutable.Map()))
        )
      else
      // v = par1 + par2 => par2 = v - par1
      // v = par1 - par2 => par2 = par1 - v
      // v = par1 * par2 => par2 = v / par1
      // v = par1 / par2 => par2 = par1 / v
      if func.equals("+") || func.equals("*") then
        shouldHave(
          monkeys,
          par2,
          parseOpInverse(func)(value, eval(monkeys, par1, mutable.Map()))
        )
      else
        shouldHave(
          monkeys,
          par2,
          parseOp(func)(eval(monkeys, par1, mutable.Map()), value)
        )
    }

@main def hello: Unit =
  val monkeys = parseInput
  println(eval(monkeys, "root", mutable.Map()))
  val newMonkeys =
    monkeys.updated("root", copy(monkeys("root"), "-"))
  val newNewMonkeys = newMonkeys.updated("humn", Input())
  println(shouldHave(newNewMonkeys, "root", 0))
