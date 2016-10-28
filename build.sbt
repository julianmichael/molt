lazy val root = project.in(file("."))
  .aggregate(moltJVM, moltJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val molt = crossProject.settings(
  name := "molt",
  organization := "org.me",
  version := "0.1-SNAPSHOT",
  libraryDependencies ++= Seq(
    "org.me" %%% "ordered" % "0.1-SNAPSHOT",
    "org.scalaz" %%% "scalaz-core" % "7.2.4",
    "com.chuusai" %%% "shapeless" % "2.3.2",
    "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
  ),
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")
)

lazy val moltJVM = molt.jvm
lazy val moltJS = molt.js
