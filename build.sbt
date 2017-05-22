lazy val root =
  project
    .in(file("."))
    .aggregate(`getting-started`, `functional-data-structures`, `handling-errors-without-exceptions`)

lazy val `getting-started` =
  project
    .in(file("getting-started"))
    .settings(
      initialCommands := """|import es.beikern.functional.programming._
                              |""".stripMargin,
      libraryDependencies ++= Vector(
        Library.scalaTest % "test"
      )
    )
    .enablePlugins(AutomateHeaderPlugin, GitVersioning)

lazy val `functional-data-structures` =
  project
    .in(file("functional-data-structures"))
    .settings(
      initialCommands := """|import es.beikern.functional.programming._
                            |""".stripMargin,
      libraryDependencies ++= Vector(
        Library.scalaTest % "test"
      )
    )
    .enablePlugins(AutomateHeaderPlugin, GitVersioning)

lazy val `handling-errors-without-exceptions` =
  project
    .in(file("handling-errors-without-exceptions"))
    .settings(
      initialCommands := """|import es.beikern.functional.programming._
                            |""".stripMargin,
      libraryDependencies ++= Vector(
        Library.scalaTest % "test"
      )
    )
    .enablePlugins(AutomateHeaderPlugin, GitVersioning)

name := "functional-programming-scala"
