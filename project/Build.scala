import sbt._
import Keys._

object MyBuild extends Build {
  lazy val parsing = Project("parsing", file(".")).
    dependsOn(streams % "compile;test;test->test")
  lazy val streams = RootProject( file("lib/sorted-streams") )
}