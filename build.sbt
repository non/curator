name := "curator"

organization := "org.spire-math"

version := "0.0.1"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.11.0")

libraryDependencies ++= Seq(
  "ichi.bench" % "thyme" % "0.1.1" from "http://plastic-idolatry.com/jars/thyme-0.1.1.jar",
  "org.spire-math" %% "spire" % "0.7.4",
  "org.scalacheck" %% "scalacheck" % "1.11.3",
  "org.scalatest" %% "scalatest" % "2.1.3"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature"
)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

seq(bintrayResolverSettings: _*)

seq(bintrayPublishSettings: _*)
