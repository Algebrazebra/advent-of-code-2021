package day2

import org.scalatest.FunSuite


class DiveTests extends FunSuite {

  val pos: (Int, Int) = (0, 0)
  val posWithAim: (Int, Int, Int) = (0, 0, 0)
  val sampleInstructions = List(
    ("forward", 5),
    ("down", 5),
    ("forward", 8),
    ("up", 3),
    ("down", 8),
    ("forward", 2),
  )

  test("Processing actions adjust returns correctly adapted positions.") {
    assert(Dive.processAction(pos, ("up", 1)) == (0, -1))
    assert(Dive.processAction(pos, ("down", 1)) == (0, 1))
    assert(Dive.processAction(pos, ("forward", 1)) == (1, 0))
  }

  test("Processing actions with value of zero does not change original position.") {
    assert(Dive.processAction(pos, ("up", 0)) == (0,0))
    assert(Dive.processAction(pos, ("down", 0)) == (0,0))
    assert(Dive.processAction(pos, ("forward", 0)) == (0,0))
  }

  test("Processing sample instruction set returns correct position.") {
    assert(Dive.processActionList(pos, sampleInstructions) == (15, 10))
  }

  test("Processing actions with aim returns correctly adapted positions") {
    assert(Dive.processActionWithAim(posWithAim, ("up", 1)) == (0, 0, -1))
    assert(Dive.processActionWithAim(posWithAim, ("down", 1)) == (0, 0, 1))
    assert(Dive.processActionWithAim(posWithAim, ("forward", 1)) == (1, 1*0, 0))
  }

  test("Processing sample instruction set with aim returns correct position.") {
    assert(Dive.processActionListWithAim(posWithAim, sampleInstructions) == (15, 60, 10))
  }

}
