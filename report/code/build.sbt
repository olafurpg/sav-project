val verify = Def.taskKey[Unit]("Run Unit tests and Leon verification")
watchSources += new File("leonVerify.sh")
verify in Compile := {
  (test in Test).value
  "./leonVerify.sh" !
}

