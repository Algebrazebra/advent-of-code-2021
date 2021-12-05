package day1

import scala.annotation.tailrec
import scala.io.Source.fromFile

object SonarSweep {

  def readData(filePath: String = "./data/day1.txt"): List[Int] = {
    val bufferedSource = fromFile(filePath)
    val depths = bufferedSource.getLines().flatMap(_.toIntOption).toList
    bufferedSource.close()
    depths
  }

  def countIncreases(values: List[Int]): Int = {
    @tailrec
    def count(values: List[Int], acc: Int): Int = values match {
      case Nil => acc
      case a::Nil => acc
      case a::b::tail => {
        if (a<b) count(b::tail, acc + 1)
        else count(b::tail, acc)
      }
    }
    count(values=values, acc=0)
  }

  def applySlidingWindow(values: List[Int]): List[Int] = values.sliding(3, 1).filter(_.length==3).map(_.sum).toList

  def solvePart1(): Int = {
    val depths = readData()
    countIncreases(depths)
  }

  def solvePart2(): Int = {
    val depths = readData()
    val windowedDepths = applySlidingWindow(depths)
    countIncreases(windowedDepths)
  }
}
