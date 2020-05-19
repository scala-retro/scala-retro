import Keys._
import ReleaseTransformations._

lazy val scala213 = "2.13.2"
lazy val scala212 = "2.12.11"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala213, scala212, scala211)

ThisBuild / organization  := "com.github.acout"
ThisBuild / scalaVersion  := "2.11.12"
ThisBuild / crossScalaVersions := supportedScalaVersions
ThisBuild / publishArtifact := true // Enable publish
ThisBuild / publishMavenStyle := true
// http://www.scala-sbt.org/0.12.2/docs/Detailed-Topics/Artifacts.html
ThisBuild / publishArtifact in Test := false
// Bintray
ThisBuild / bintrayPackageLabels := Seq("scala", "sbt")
ThisBuild / bintrayRepository := "maven"
ThisBuild / bintrayOrganization := Some("anthonycoutant")
ThisBuild / developers := List(
  Developer(
    id    = "acout",
    name  = "Anthony Coutant",
    email = "ac@anthonycoutant.me",
    url   = url("http://anthonycoutant.me")
  )
)
ThisBuild / licenses += ("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

lazy val core = (project in file("core"))
  .settings(
    name := "scala-retro-core",
    description := "Scala Retro Core",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.10"
  )

lazy val cli = (project in file("cli"))
  .settings(
    name := "scala-retro-cli",
    description := "Scala Retro Cli"
  ).dependsOn(core)

lazy val root = (project in file("."))
  .aggregate(core, cli)
  .settings(
    name := "scala-retro",
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip := true,
    // don't use sbt-release's cross facility
    releaseCrossBuild := false,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      releaseStepCommandAndRemaining("+test"),
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommandAndRemaining("+publish"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}