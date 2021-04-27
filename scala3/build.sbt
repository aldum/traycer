val dottyVersion = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-traycer",
    description := "Ray Tracer implemented in Scala 3",
    version := "0.1.0",

    organization := "pw.aldum",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.3",
      "org.scalatest" %% "scalatest" % "3.2.7"
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework"),

    useScala3doc := true,
  )
