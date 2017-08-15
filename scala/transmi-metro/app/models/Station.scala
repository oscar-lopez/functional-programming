package models

import rx.lang.scala.Observable

import scala.collection.mutable

class Station(val name: String, hub: Boolean, tm: TransmiMetro) {

  // passengers in this station waiting to board cars, in FIFO order
  val passengers: mutable.Queue[Passenger] = mutable.Queue[Passenger]()

  def simulate(time: String, arrivedCars: Observable[Int], arrivedPassengers: Observable[Passenger]): Unit = {

    var passList = List[Passenger]()
    arrivedPassengers.subscribe(passenger => passList :+= passenger)
    passengers.enqueue(passList: _*)

    arrivedCars.subscribe(carId => {
      val car = tm.getCar(carId)
      car.currentStation = name
      val available = car.availableSeats()
      val num = Math.min(passengers.size, available)
      val boardingPassengers = (1 to num).map(_ => passengers.dequeue())
      car.simulate(time, boardingPassengers)
    })

    tm.log(time, name, "arrivals", passList.size)
    tm.log(time, name, "density", passengers.size)

  }

}
