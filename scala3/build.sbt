val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-traycer",
    description := "Ray Tracer implemented in Scala 3",
    version := "0.1.0",

    organization := "pw.aldum",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework"),

  )
