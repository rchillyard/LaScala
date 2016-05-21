organization := "com.phasmid"

name := "laScala"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

val scalaTestVersion = "2.2.6"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
