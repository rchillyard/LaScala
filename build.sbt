organization := "com.phasmid"

name := "lascala"

// Build Scala 3 with the oldest supported LTS so that the artifact is usable from any later Scala 3 version.
// CircleCI also runs the tests on the current LTS (3.9).
val scala213 = "2.13.18"
val scala3 = "3.3.8"

scalaVersion := scala213
crossScalaVersions := Seq(scala213, scala3)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)

lazy val IT = config("it") extend Test

lazy val lascala = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .configs(IT)
  .settings(
    inConfig(IT)(Defaults.testSettings),
    IT / scalaSource := baseDirectory.value / "src" / "it" / "scala",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "lascalabuildinfo"
  )

buildInfoKeys ++= Seq[BuildInfoKey](
  BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  } // re-computed each time at compile
)

val scalaModules = "org.scala-lang.modules"

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  scalaModules %% "scala-parser-combinators" % "2.4.0"
  , scalaModules %% "scala-collection-compat" % "2.14.0"
  , "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"
  , "com.typesafe" % "config" % "1.4.9"
  , "ch.qos.logback" % "logback-classic" % "1.5.38" % Runtime
  // NOTE: Akka is used only for testing this package.
  , "com.typesafe.akka" %% "akka-actor" % "2.6.21" % Test
  , "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
  , "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  // NOTE: only used for testing
  , "junit" % "junit" % "4.13.2" % Test
)

// Runtime reflection (used by RenderableCaseClass) exists only in Scala 2.
libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  case _ => Seq()
})
