ThisBuild / organization := "pw.aldum"
ThisBuild / scalaVersion := "3.0.0"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalacOptions ++=
  Seq(
    // "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:adhocExtensions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Ykind-projector",
    // "-Ysafe-init", // experimental (I've seen it cause issues with circe)
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")

lazy val `scala3-traycer` =
  project
    .in(file("."))
    .settings(name := "scala3-traycer")
    .settings(description := "Ray Tracer implemented in Scala 3")
    .settings(commonSettings)
    .settings(dependencies)

lazy val commonSettings = Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty,
  Compile / console / scalacOptions --= Seq(
    "-Wunused:_",
    "-Xfatal-warnings",
  ),
  Test / console / scalacOptions :=
    (Compile / console / scalacOptions).value,
)

lazy val dependencies = Seq(
  libraryDependencies ++= Seq(
    // main dependencies
  ),
  libraryDependencies ++= Seq(
    // "org.scalacheck" %% "scalacheck" % "1.15.4",
    "org.scalatest" %% "scalatest" % "3.2.9",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0",
  ).map(_ % Test),
)
