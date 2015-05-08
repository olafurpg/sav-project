name := "leon-go"

organization := "ch.epfl"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
  ,  "com.lihaoyi" %% "ammonite-ops" % "0.3.0"
)

val leonVerification = Def.taskKey[Unit]("Verify leon files")

leonVerification in Compile := {
  // We execute the scala tests before running leon verification
  (test in Test).value
  "./leonVerify.sh" !
}


initialCommands := "import ch.epfl.leongo._"

