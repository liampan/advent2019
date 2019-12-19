package adventservices

import org.scalatest.{MustMatchers, WordSpec}

class DayOneSpec extends WordSpec with MustMatchers {

  "CountFuelFor" must {
    "return 2" when {
      "mass of 12" in {
        DayOne.countFuelFor(12) mustBe  2
      }

      "mass is 14" in {
        DayOne.countFuelFor(14) mustBe  2
      }
    }

    "return 654" when {
      "mass of 1969" in {
        DayOne.countFuelFor(1969) mustBe 654
      }
    }

    "For a mass of 100756, the fuel required is 33583." in {
      DayOne.countFuelFor(100756) mustBe 33583
    }
  }

  "getAllRocketFuel" must {
    "total all rocket mass" in {
      DayOne.getAllRocketFuel() mustBe 5011553
    }
  }

  "addOnAdditionalFuel" must {
    "return 0 " when {
      "an initial 2 fuel is required" in {
        DayOne.addOnAdditionalFuel(2) mustBe 2
      }
    }
    "return 966" when {
      "an initial 654 fuel is required" in {
        DayOne.addOnAdditionalFuel(654) mustBe 966
      }
    }
    "return 50346" when {
      "a module has mass of 100756 which needs initial of 33583" in {
        val initialFuel = DayOne.countFuelFor(100756)
        initialFuel mustBe 33583
        DayOne.addOnAdditionalFuel(initialFuel) mustBe 50346
      }
    }
  }


  //A module of mass 14 requires 2 fuel. This fuel requires no further
  // fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel),
  // so the total fuel required is still just 2.

  //At first, a module of mass 1969 requires 654 fuel.
  // Then, this fuel requires 216 more fuel (654 / 3 - 2).
  // 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel,
  // which requires no further fuel.
  // So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.

  //The fuel required by a module of mass 100756 and its fuel is:
  // 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346
}
