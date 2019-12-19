package adventservices

import util.InputReader

object DayOne {


  //Fuel required to launch a given module is based on its mass.
  // Specifically, to find the fuel required for a module,
  // take its mass, divide by three, round down, and subtract 2.

  private[adventservices] def countFuelFor(moduleMass: Int): Int =
    if (moduleMass>6) (moduleMass/3)-2 else 0


  def getAllRocketFuel(): Int = {
    val file = InputReader.getInput("day_1_input")
    file.foldLeft(0)((acc, cur) => acc + addOnAdditionalFuel(countFuelFor(cur.toInt)))
  }

  //So, for each module mass, calculate its fuel and add it to the total.
  // Then, treat the fuel amount you just calculated as the input mass
  // and repeat the process, continuing until a fuel requirement
  // is zero or negative.

  def addOnAdditionalFuel(dryMassRequiredFuel: Int) : Int = {
    def additionalFuelHelper(dryMassRequiredFuel: Int, acc: Int): Int = {
      countFuelFor(dryMassRequiredFuel) match {
        case 0 => dryMassRequiredFuel + acc
        case extraFuel => additionalFuelHelper(extraFuel, dryMassRequiredFuel + acc)
      }
    }
    additionalFuelHelper(dryMassRequiredFuel, 0)
  }

}
