package models

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class CarItinerary(var carId: Int, var maxCapacity: Int, var time: String, var departure: String, var destination: String) {
}

class Schedule {

  // { car_id => { time, itinerary } }
  val schedule: mutable.Map[Int, mutable.Map[String, CarItinerary]] =
    mutable.Map[Int, mutable.Map[String, CarItinerary]]().withDefaultValue(mutable.Map[String, CarItinerary]())

  def readSchedule(): mutable.Map[Int, mutable.Map[String, CarItinerary]] = {

    val rows = ArrayBuffer[Array[String]]()
    using(Source.fromFile(TransmiMetro.DATA_PATH + "schedules.csv")) { source =>
      for (line <- source.getLines.drop(1)) {
        rows += line.split(",").map(_.trim)
      }
    }

    for (row <- rows) {
      val carId = row(0).toInt
      val maxCapacity = row(1).toInt
      val time = row(3)
      val departure = row(4)
      val destination = row(1)
      addCarItinerary(carId, maxCapacity, time, departure, destination)
    }

    schedule

  }

  // private helpers

  private def addCarItinerary(carId: Int, maxCapacity: Int, time: String, departure: String, destination: String): Unit = {
    val itinerary = new CarItinerary(carId, maxCapacity, time, departure, destination)
    schedule(carId).put(time, itinerary)
  }

  private def using[A <: {def close(): Unit}, B]
  (resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

}
