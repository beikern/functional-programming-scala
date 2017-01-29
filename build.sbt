lazy val `functional-programming-scala` =
  project.in(file(".")).enablePlugins(AutomateHeaderPlugin, GitVersioning)

libraryDependencies ++= Vector(
  Library.scalaTest % "test"
)

initialCommands := """|import es.beikern.functional.programming.scala._
                      |""".stripMargin
