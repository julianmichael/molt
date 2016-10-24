import sbt._
import Keys._

object MyBuild extends Build {
  lazy val ordered = RootProject( file("lib/ordered") )
  lazy val molt = Project("molt", file(".")).
    dependsOn(ordered % "compile;test;test->test")
}
