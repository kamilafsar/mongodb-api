import sbt._
import sbt.Keys._

object ApplicationBuild extends Build {

  val _version = "1.0"
  val _scalaVersion = "2.11.1"
  val _scalacOpts = Seq("-deprecation", "-feature")
  val _resolvers = Seq(
    "Typesafe releases repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Typesafe snaphots repository" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
  )
  val reactiveMongoDepenency = "org.reactivemongo" %% "reactivemongo" % "0.11.0-SNAPSHOT"

  def module(moduleName: String) = {
    Project(moduleName, file(s"modules/$moduleName"))
      .settings(
        name := moduleName,
        version := _version,
        scalaVersion := _scalaVersion,
        scalacOptions ++= _scalacOpts,
        resolvers ++= _resolvers,
        doc in Compile <<= target.map(_ / "none")
      )
  }


  val macros = module("macros")
    .settings(
      sourceDirectory in Compile <<= baseDirectory,
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % "2.11.1",
        reactiveMongoDepenency
      )
    )


  val main = Project("""mongodb-api""", file("."))
    .settings(
      version := _version,
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.1.6" % "test",
        "org.specs2" %% "specs2" % "2.4.5" % "test",
        "com.typesafe.akka" %% "akka-actor" % "2.3.3",
        reactiveMongoDepenency
      ),
      resolvers ++= _resolvers,
      fork in Test := false,
      scalacOptions ++= _scalacOpts,
      scalaVersion := _scalaVersion,
      doc in Compile <<= target.map(_ / "none")
    )
    .dependsOn(macros)
}
