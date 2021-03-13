scalaVersion := "2.13.3"

name := "scalabootcamp-hw"
organization := "com.evolution"
version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.scalatest" %% "scalatest" % "latest.integration" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "latest.integration" % "test",
)
