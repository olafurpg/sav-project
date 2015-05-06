name := "leon-go"

organization := "ch.epfl"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
)

initialCommands := "import ch.epfl.leongo._"
