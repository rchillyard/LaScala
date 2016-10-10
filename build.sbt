organization := "com.phasmid"

name := "lascala"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

val scalaTestVersion = "2.2.6"
val scalaModules = "org.scala-lang.modules"
val scalaModulesVersion = "1.0.4"

// NOTE: Akka is used only for testing this package.
val akkaGroup = "com.typesafe.akka"
val akkaVersion = "2.4.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  scalaModules %% "scala-parser-combinators" % scalaModulesVersion,
  // TODO we don't need this but dependencies apparently use different versions:
  scalaModules %% "scala-xml" % scalaModulesVersion,
  // TODO merge date/time code so as to use Java8 instead of Joda-time
  "codes.reactive" %% "scala-time" % "0.4.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7" % "runtime",
  // NOTE: only used for testing
  akkaGroup %% "akka-actor" % akkaVersion % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
