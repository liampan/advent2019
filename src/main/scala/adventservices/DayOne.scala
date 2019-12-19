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
    file.foldLeft(0)((acc, cur) => acc + countFuelFor(cur.toInt))
  }

  def additionalFuel(dryMassRequiredFuel: Int) : Int = {
    countFuelFor(dryMassRequiredFuel) match {
      case 0 => dryMassRequiredFuel
    }
  }

}
