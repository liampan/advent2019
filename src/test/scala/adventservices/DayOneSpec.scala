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
      DayOne.getAllRocketFuel() mustBe 3342946
    }
  }

}
