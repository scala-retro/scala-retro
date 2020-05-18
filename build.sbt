import Keys._

name := "scala-retro"

version := "0.1.1"

scalaVersion := "2.11.12"

lazy val scala213 = "2.13.2"
lazy val scala212 = "2.12.11"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala213, scala212, scala211)

crossScalaVersions := supportedScalaVersions

libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.10"

organization := "com.github.acout"
licenses += ("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
bintrayRepository := "maven"
bintrayOrganization := Some("anthonycoutant")

developers := List(
  Developer(
    id    = "acout",
    name  = "Anthony Coutant",
    email = "ac@anthonycoutant.me",
    url   = url("http://anthonycoutant.me")
  )
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}