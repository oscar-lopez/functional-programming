package models

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class DataReader {

  val stations_dir: String = TransmiMetro.DATA_PATH + "stations"

  def getStationList: List[String] = {
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    getListOfFiles(stations_dir)
      .map(x => x.toString)
      .map(x => x.substring(0, x.lastIndexOf('.')))
      .map(x => x.substring(x.lastIndexOf("/") + 1))
  }

  def getPassengers(station: String): List[Passenger] = {

    val rows = ArrayBuffer[Array[String]]()
    val passengers = mutable.Buffer[Passenger]()

    using(Source.fromFile(stations_dir + "/" + station + ".csv")) { source =>
      for (line <- source.getLines.drop(1)) {
        rows += line.split(",").map(_.trim)
      }
    }

    for (row <- rows) {
      val passengerId = row(0).toInt
      val time = row(1)
      val destination = row(2)
      passengers.append(new Passenger(passengerId, time, destination))
    }

    passengers.toList

  }

  // private helpers

  private def using[A <: {def close(): Unit}, B]
  (resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

}
