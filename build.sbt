organization := "com.phasmid"

name := "lascala"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

val scalaTestVersion = "2.2.6"

// Akka is used only for testing this package.
val akkaGroup = "com.typesafe.akka"
val akkaVersion = "2.4.1"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  // TODO merge date/time code so as to use Java8 instead of Joda-time
  "codes.reactive" %% "scala-time" % "0.4.0",
  // TODO for now, until we can convert to Java8 datetimes everywhere
  "joda-time" % "joda-time" % "2.9.2",
  //  "com.github.nscala-time" %% "nscala-time" % "2.12.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7" % "runtime",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  akkaGroup %% "akka-actor" % akkaVersion % "test",
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
