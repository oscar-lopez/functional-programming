
name := """transmi-metro"""
organization := "co.edu.uniandes"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.2"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.0" % Test



// Adds additional packages into Twirl
//TwirlKeys.templateImports += "co.edu.uniandes.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "co.edu.uniandes.binders._"
