name := "leon-go"

organization := "ch.epfl"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

persistLauncher in Compile := true

persistLauncher in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
  ,  "com.lihaoyi" %% "ammonite-ops" % "0.3.0"
)

val verify = Def.taskKey[Unit]("Run unit tests and leon verification")

watchSources += new File("leonVerify.sh")

verify in Compile := {
  (test in Test).value
  "./leonVerify.sh" !
}


initialCommands := "import go._"

