import sbt._
import Keys._

object RuleNLGBuild extends Build {

  import Dependencies._

  val buildSettings = Seq(
    organization := "rodneykinney",
    name := "rulenlg",
    version := "0.0.1-SNAPSHOT",

    scalaVersion := "2.10.4",

    scalacOptions ++= List(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-encoding", "UTF-8"
    ),

    conflictManager := ConflictManager.strict,

    resolvers ++= Dependencies.defaultResolvers,

    dependencyOverrides ++= Set(
    ),

    libraryDependencies ++= Seq(
      sprayJson
    )
  )
  lazy val ruleNLG = Project(
    id = "rulenlg",
    base = file("."),
    settings = buildSettings
  )
}
