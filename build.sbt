lazy val root = (project in file(".")).settings(
  inThisBuild(List(
    organization := "com.example",
    scalaVersion := "2.12.7",
    version      := "0.1.0-SNAPSHOT"
  )),
  name := "busywork",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.0.5" % Test
  ),
  scalacOptions += "-feature",
  scalacOptions += "-unchecked",
)
