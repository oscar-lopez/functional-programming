package models

class Car(val id: Int, val capacity: Int, var currentStation: String, tm: TransmiMetro) {

  // passengers currently in this car
  var passengers: List[Passenger] = List[Passenger]()

  // pre: the arrived list fits in the car's capacity
  def simulate(time: String, boardingPassengers: Iterable[Passenger]): Unit = {
    val departures = passengers.count(_.destination == currentStation)
    tm.log(time, currentStation, "departures", departures)
    passengers = passengers.filter(_.destination != currentStation)
    passengers ++= boardingPassengers
  }

  def availableSeats(): Int = {
    val departures = passengers.count(_.destination == currentStation)
    passengers.size - departures
  }



}
