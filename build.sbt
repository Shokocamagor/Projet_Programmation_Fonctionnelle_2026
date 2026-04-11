ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "ant-colony",
    version := "0.1.0",

    Compile / scalaSource := baseDirectory.value / "app" / "src" / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "app" / "src" / "main" / "resources",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.5",
      "com.typesafe.akka" %% "akka-stream" % "2.8.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11"
    )
  )