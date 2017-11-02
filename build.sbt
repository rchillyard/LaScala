organization := "com.phasmid"

name := "lascala"

scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.10.6","2.11.8"
//  ,"2.12.3"
 )

lazy val lascala = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "lascalabuildinfo"
  )

buildInfoKeys ++= Seq[BuildInfoKey](
  BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  } // re-computed each time at compile
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

val scalaModules = "org.scala-lang.modules"
val scalaModulesVersion = "1.0.4"

val scalaCheckGroup = "org.scalacheck"
val scalaCheckArt = "scalacheck"

val typesafeGroup = "com.typesafe"
val configVersion = "1.3.1"
// NOTE: Akka is used only for testing this package.
val akkaGroup = "com.typesafe.akka"
lazy val akkaVersion = SettingKey[String]("akkaVersion")
lazy val scalaTestVersion = SettingKey[String]("scalaTestVersion")

akkaVersion := (scalaBinaryVersion.value match {
  case "2.10" => "2.3.15"
  case "2.11" => "2.4.1"
  case "2.12" => "2.5.4"
})
scalaTestVersion := (scalaBinaryVersion.value match {
  case "2.10" => "2.2.6"
  case "2.11" => "3.0.1"
  case "2.12" => "3.0.1"
})

libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.12" =>   Seq(
    scalaModules %% "scala-parser-combinators" % scalaModulesVersion,
    // NOTE: we don't need this but dependencies apparently use different versions:
    scalaModules %% "scala-xml" % "1.0.6",
    akkaGroup %% "akka-actor" % akkaVersion.value % "test",
    scalaCheckGroup %% scalaCheckArt % "1.13.4" % "test",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.0"
  )
    case "2.11" =>   Seq(
      scalaModules %% "scala-parser-combinators" % scalaModulesVersion,
      // NOTE: we don't need this but dependencies apparently use different versions:
      scalaModules %% "scala-xml" % scalaModulesVersion,
      akkaGroup %% "akka-actor" % akkaVersion.value % "test",
      scalaCheckGroup %% scalaCheckArt % "1.12.6" % "test",
      "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
    )
  case "2.10" =>   Seq(
    akkaGroup %% "akka-actor" % akkaVersion.value % "test",
    scalaCheckGroup %% scalaCheckArt % "1.12.6" % "test",
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  )
    case _ => Seq()
  })

libraryDependencies ++= Seq(
  typesafeGroup % "config" % configVersion,
  "ch.qos.logback" %  "logback-classic" % "1.1.7" % "runtime",
  // NOTE: only used for testing
  "junit" % "junit" % "4.11" % Test,
  "org.scalatest" %% "scalatest" % scalaTestVersion.value % "test"
)

releaseCrossBuild := true

publishTo := Some(Resolver.file("file",  new File( "artifacts" )) )
