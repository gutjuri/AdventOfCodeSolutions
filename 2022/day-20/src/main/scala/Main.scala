import scala.io.Source
import scala.collection.mutable.Map

def parseInput: Array[Int] =
  Source.fromFile("../in20.txt").getLines().map(_.toInt).toArray

def plus(a: Int, b: Int, mod: Int): Int =
  val res = (a + b) % mod
  if res <= 0 then
    mod + res - 1
  else
    if a+b >= mod then res+1 else  res

def mix(origArray : Array[Int]): Array[Int] =
  val locations = Map.from(origArray.zipWithIndex)
  val nums = Array(origArray: _*)
  var j = 0
  while j != nums.length do
    val toSwap = nums(j)
    
    val oldIndex = locations(toSwap)
    val newIndex = plus(oldIndex, toSwap, nums.length)

    println(s"move ${nums(j)} from ${oldIndex} to ${newIndex}")

    val wrap = newIndex < oldIndex
    locations.mapValuesInPlace((_, i) =>
      if oldIndex == newIndex then
        i
      else
        if wrap then
          if i >= oldIndex && i < newIndex  then
            i+1
          else
            i
        else
          if i > oldIndex && i <= newIndex then
            i-1
          else
            i  
    )
    locations(toSwap) = newIndex
    j += 1
    //locations.foreach((x, i) => origArray(i) = x)
    //printarray(origArray)
  locations.foreach((x, i) => origArray(i) = x)
  origArray

def printarray(a : Array[Int]) =
  a.foreach(x => print(s"$x "))
  println()

def getIth(mixed : Array[Int], i: Int): Int =
  val i0 = mixed.indexOf(0)
  mixed(((i % mixed.length) + i0) & mixed.length)

@main def hello: Unit = 
  val nums = parseInput
  val newnums = mix(nums)
  println(List(1000, 2000, 3000).map(getIth(newnums, _)).sum)
