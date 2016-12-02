organization := "com.phasmid"

name := "lascala"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

val scalaTestVersion = "3.0.1"
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

// The following values can be used for Scala 2.10 although bear in mind that some parts of the code
// will only compile in 2.11. If you don't need these parts of the code, then you can build with 2.10
// simply by fixing the logging imports.

//scalaVersion := "2.10.6"
//
//val scalaTestVersion = "2.2.6"
//
//// NOTE: Akka is used only for testing this package.
//val akkaGroup = "com.typesafe.akka"
//val akkaVersion = "2.3.15"
//
//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//
//libraryDependencies ++= Seq(
//  "codes.reactive" %% "scala-time" % "0.4.0",
//  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
//  "ch.qos.logback" %  "logback-classic" % "1.1.7" % "runtime",
//  akkaGroup %% "akka-actor" % akkaVersion % "test",
//  "org.scalacheck" %% "scalacheck" % scalaTestVersion % "test",
//  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
//)
