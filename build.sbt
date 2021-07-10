lazy val root = project
  .in(file("."))
  .settings(
    name := "icfpc-2021",
    version := "0.1",
    scalaVersion := "2.13.6",
    libraryDependencies ++= {
      val javaFXModules = Seq(
        "base",
        "controls",
        "fxml",
        "graphics",
        "media",
        "swing",
        "web"
      )
      val circeModules = Seq(
        "circe-core",
        "circe-generic",
        "circe-parser"
      )
      Seq(
        "org.scalafx" %% "scalafx" % "16.0.0-R24",
        "org.scalatest" %% "scalatest" % "3.2.9" % "test",
        "org.typelevel" %% "cats-core" % "2.3.0"
      ) ++ javaFXModules.map(m => "org.openjfx" % s"javafx-$m" % "16" classifier "linux") ++
        circeModules.map(m => "io.circe" %% m % "0.14.1")
    }
  )
