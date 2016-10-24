name := "molt"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.4",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq("-feature", "-language:higherKinds")

