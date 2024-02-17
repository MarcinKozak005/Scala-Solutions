import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class P12_RobotNameTest extends AnyFunSuite {

  test("Numbers should be correctly converted to letter-based Base 26") {
    assert(RobotP12.paddedBase26Letter(0) == "A")
    assert(RobotP12.paddedBase26Letter(1) == "B")
    assert(RobotP12.paddedBase26Letter(26) == "BA")
    assert(RobotP12.paddedBase26Letter(675) == "ZZ")
    assert(RobotP12.paddedBase26Letter(1000) == "BMM")
  }

  test("Letter-based Base 26 numbers should be correctly padded") {
    assert(RobotP12.paddedBase26Letter(0,3) == "AAA")
    assert(RobotP12.paddedBase26Letter(1,1) == "B")
    assert(RobotP12.paddedBase26Letter(26,5) == "AAABA")
  }

  // from exercism
  test("a large number of new instances have unique names") {
    val alreadySet = mutable.HashSet.empty[String]
    for (_ <- 0 until 676000) {
      val name = new RobotP12().name
      if (alreadySet contains name) {
        fail(s"$name is repeated")
      }
      alreadySet += name
    }
  }

}
