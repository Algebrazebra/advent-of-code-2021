package day3

import org.scalatest.FunSuite


class BinaryDiagnosticsTests extends FunSuite {

  val sampleData = List(
    "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"
  )

  test("Summing the bit positions returns correct result") {
    assert(BinaryDiagnostic.sumBits(sampleData) == List(7, 5, 8, 7, 5))
  }

  test("Gamma und epsilon is correctly calculated on sample data.") {
    assert(BinaryDiagnostic.calculateGammaEpsilon(sampleData) == (22, 9))
  }

  test("Reading ratings in sample data returns correct result.") {
    assert(BinaryDiagnostic.readRatings(sampleData) == (23, 10))
  }

}
