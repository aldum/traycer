lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-traycer",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    organization := "pw.aldum",
    scalaVersion := "3.0.0-M3",
    libraryDependencies ++= Seq(
      "org.scalacheck" % "scalacheck_3.0.0-M3" % "1.15.2",
      "org.scalatest" % "scalatest_3.0.0-M3" % "3.2.3"
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework"),

    useScala3doc := true,
  )
