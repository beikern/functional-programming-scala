
lazy val root =
  project.in(file("."))
    .aggregate(`getting-started`, `functional-data-structures`)

lazy val `getting-started` =
  project.in(file("getting-started"))
      .settings(
        initialCommands := """|import es.beikern.functional.programming._
                              |""".stripMargin,
        libraryDependencies ++= Vector(
          Library.scalaTest % "test"
        )
      )
    .enablePlugins(AutomateHeaderPlugin, GitVersioning)

lazy val `functional-data-structures` =
  project.in(file("functional-data-structures"))
    .settings(
      initialCommands := """|import es.beikern.functional.programming._
                            |""".stripMargin,
      libraryDependencies ++= Vector(
        Library.scalaTest % "test"
      )
    )
    .enablePlugins(AutomateHeaderPlugin, GitVersioning)

name := "functional-programming-scala"

