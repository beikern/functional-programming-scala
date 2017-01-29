
lazy val root =
  project.in(file("."))
    .aggregate(`getting-started`)

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

name := "root"

