package day1

import org.scalatest.FunSuite

class SonarSweepTests extends FunSuite {

  val sampleData = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  test("Zero increases for empty list") {
    assert(SonarSweep.countIncreases(List.empty) == 0)
  }

  test("Zero increases for list with one element") {
    assert(SonarSweep.countIncreases(List(0)) == 0)
  }

  test("Sample data has 7 increases") {
    assert(SonarSweep.countIncreases(sampleData) == 7)
  }

  test("applySlidingWindow yields empty list on lists smaller than window size") {
    assert(SonarSweep.applySlidingWindow(List.empty) == List.empty)
    assert(SonarSweep.applySlidingWindow(List(1)) == List.empty)
    assert(SonarSweep.applySlidingWindow(List(1, 2)) == List.empty)
  }

  test("applySlidingWindows only yields windows of size 3") {
    assert(SonarSweep.applySlidingWindow(List(1, 2, 3, 4)).length == 2)
  }

  test("Count increases with sliding windows on sample data yields 5 increases") {
    val windowed = SonarSweep.applySlidingWindow(sampleData)
    assert(SonarSweep.countIncreases(windowed) == 5)
  }

}
