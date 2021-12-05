package day2

import scala.annotation.tailrec
import scala.io.Source.fromFile

object Dive {

  def readData(filePath: String = "./data/day2.txt"): List[(String, Int)] = {
    val bufferedSource = fromFile(filePath)
    val actions = bufferedSource
      .getLines()
      .map(_.split(" "))
      .map({ case Array(a, b) => (a, b.toIntOption.get) })
      .toList
    bufferedSource.close()
    actions
  }

  def processAction(currPos: (Int, Int), action: (String, Int)): (Int, Int) = action match {
    case ("forward", value) => (currPos._1 + value, currPos._2)
    case ("down", value) => (currPos._1, currPos._2 + value)
    case ("up", value) => (currPos._1, currPos._2 - value)
  }

  def processActionWithAim(currPos: (Int, Int, Int), action: (String, Int)): (Int, Int, Int) = action match {
    case ("forward", value) => (currPos._1 + value, currPos._2 + value * currPos._3, currPos._3)
    case ("down", value) => (currPos._1, currPos._2, currPos._3 + value)
    case ("up", value) => (currPos._1, currPos._2, currPos._3 - value)
  }

  @tailrec
  def processActionList(currPos: (Int, Int), actions: List[(String, Int)]): (Int, Int) = actions match {
    case Nil => currPos
    case action::tail => processActionList(processAction(currPos, action), tail)
  }

  @tailrec
  def processActionListWithAim(currPos: (Int, Int, Int), actions: List[(String, Int)]): (Int, Int, Int) = actions match {
    case Nil => currPos
    case action::tail => processActionListWithAim(processActionWithAim(currPos, action), tail)
  }

  def solvePart1(): Int = {
    val actions = readData()
    val initialPosition = (0, 0)
    val (horizontalPos, depth) = processActionList(initialPosition, actions)
    horizontalPos * depth
  }

  def solvePart2(): Int = {
    val actions = readData()
    val initialPositionWithAim = (0, 0, 0)
    val (horizontalPos, depth, _) = processActionListWithAim(initialPositionWithAim, actions)
    horizontalPos * depth
  }

}
