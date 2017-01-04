organization := "com.phasmid"

name := "lascala"

version := "1.0.0"

scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.10.6","2.11.8")

val scalaModules = "org.scala-lang.modules"
val scalaModulesVersion = "1.0.4"

// NOTE: Akka is used only for testing this package.
val akkaGroup = "com.typesafe.akka"
lazy val akkaVersion = SettingKey[String]("akkaVersion")
lazy val scalaTestVersion = SettingKey[String]("scalaTestVersion")

akkaVersion := (scalaBinaryVersion.value match {
  case "2.10" => "2.3.15"
  case "2.11" => "2.4.1"
})
scalaTestVersion := (scalaBinaryVersion.value match {
  case "2.10" => "2.2.6"
  case "2.11" => "3.0.1"
})

libraryDependencies ++= (scalaBinaryVersion.value match {
    case "2.11" =>   Seq(
      scalaModules %% "scala-parser-combinators" % scalaModulesVersion,
      //  // TODO we don't need this but dependencies apparently use different versions:
      scalaModules %% "scala-xml" % scalaModulesVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
    )
    case _ => Seq()
  })

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  // TODO merge date/time code so as to use Java8 instead of Joda-time
  "codes.reactive" %% "scala-time" % "0.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7" % "runtime",
  // NOTE: only used for testing
  akkaGroup %% "akka-actor" % akkaVersion.value % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion.value % "test"
)

