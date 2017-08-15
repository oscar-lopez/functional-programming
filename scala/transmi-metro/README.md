# transmi-metro

Authors: Óscar López    @oscar-lopez
         Jorge Grajales @jagrales22

This project simulates the system of Transmi-metro, a transportation system with two main lines.

All schedules for car stops and passenger arrivals are read from .csv files. Passengers and cars are scheduled and then, a simulation starts moving passengers to their respective buses from 0400 to 0000 hours.

*ToDo:* Frontend part is pending. We will use observers calling `simulate()`, consumed by web sockets to refresh information as soon as it changes. Reports per station would be available to be downloaded clicking on every station name using JavaScript.

We use streams in three points in the simulation:

 - For sending each station the ids of the cars that are reaching it at each tick.
 - For sending each station the passengers that entered it at each tick.
 - For returning to the web consumer the number of arrivals, departures and density occurring in the current tick, segregated by station.

The data for the reports is obtained calling `reportDensity()`, `reportArrivals()` and `reportDepartures()` per each station, which return a map with times and values, aggregated in 10 minute intervals.
