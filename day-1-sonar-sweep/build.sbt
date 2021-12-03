name := "day-1-sonar-sweep"

version := "0.1"

scalaVersion := "2.13.7"

resourceDirectory in Compile := baseDirectory.value / "/resources"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest-flatspec" % "3.3.0-SNAP3" % Test
)