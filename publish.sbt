ThisBuild / description := "A smorgasbord of functional and general utility libraries for Scala"
ThisBuild / homepage := Some(url("https://github.com/rchillyard/LaScala"))
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

// Publishing configuration
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

// Credentials
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")

// Required POM metadata for Maven Central
ThisBuild / licenses := List("LGPL-2.1" -> url("https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/LaScala"),
    "scm:git@github.com:rchillyard/LaScala.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rchillyard",
    name  = "Robin Hillyard",
    email = "rchillyard@phasmidsoftware.com",
    url   = url("https://phasmidsoftware.com")
  )
)

// Publishing settings
ThisBuild / versionScheme := Some("early-semver")

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
