name := "leon-go"

organization := "ch.epfl"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

enablePlugins(ScalaJSPlugin)

skip in packageJSDependencies := false

scalaJSStage in Global := FastOptStage

persistLauncher in Compile := true

persistLauncher in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
  ,  "com.lihaoyi" %% "ammonite-ops" % "0.3.0"
  , "com.github.japgolly.scalajs-react" %%% "core" % "0.8.4"
  , "org.scala-js" %%% "scalajs-dom" % "0.8.0"
)

jsDependencies += "org.webjars" % "react" % "0.12.1" / "react-with-addons.js" commonJSName "React"

// Test support including ReactTestUtils
//   (requires react-with-addons.js instead of just react.js)
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "test" % "0.8.4" % "test"

// Scalaz support
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "ext-scalaz71" % "0.8.4"

// Monocle support
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "ext-monocle" % "0.8.4"

// Extra features (includes Scalaz and Monocle support)
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "extra" % "0.8.4"

val leonVerification = Def.taskKey[Unit]("Verify leon files")

watchSources += new File("leonVerify.sh")

leonVerification in Compile := {
  (test in Test).value
  "./leonVerify.sh" !
}


initialCommands := "import go._"

