package day3

import scala.annotation.tailrec
import scala.io.Source.fromFile

object BinaryDiagnostic {

  def readData(filePath: String = "./data/day3.txt"): List[String] = {
    val bufferedSource = fromFile(filePath)
    val report = bufferedSource.getLines().toList
    bufferedSource.close()
    report
  }

  def sumBits(report: List[String]): List[Int] = {
    val bitValues: List[List[Int]] = report.map(_.toList.map(_.asDigit))
    bitValues.reduceLeft((summed, binary) => {
      summed.lazyZip(binary).map(_ + _)
    })
  }

  def reverseBinaryString(binary: String): String = {
    binary
      .replaceAll("0", "x")
      .replaceAll("1", "0")
      .replaceAll("x", "1")
  }

  def calculateGammaEpsilon(report: List[String]): (Int, Int) = {
    val summedBits = sumBits(report)
    val gammaBinary: String = summedBits
      .map(x => if (x > report.length / 2) 1 else 0)
      .mkString(sep="")
    val epsilonBinary = reverseBinaryString(gammaBinary)
    val gamma = Integer.parseInt(gammaBinary, 2)
    val epsilon = Integer.parseInt(epsilonBinary, 2)
    (gamma, epsilon)
  }

  def readRatings(report: List[String]): (Int, Int) = {
    @tailrec
    def findRating(report: List[String], bitPos: Int, compare: (Int, Int) => Boolean): Int = {
      if (report.length == 1) {
        Integer.parseInt(report.head, 2)
      } else {
        val bitCounts = report.count(_.charAt(bitPos) == '1')
        val threshold = (report.length + 1) / 2
        val bit = if (compare(bitCounts, threshold)) '1' else '0'
        findRating(report.filter(_.charAt(bitPos) == bit), bitPos + 1, compare)
      }
    }
    val generatorRating = findRating(report, 0, _ >= _)
    val scrubberRating = findRating(report, 0, _ < _)
    (generatorRating, scrubberRating)
  }

  def solvePart1(): Int = {
    val report = readData()
    val (g, e) = calculateGammaEpsilon(report)
    g * e
  }

  def solvePart2(): Int = {
    val report = readData()
    val (g, s) = readRatings(report)
    g * s
  }
}
