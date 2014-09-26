name := """mongodb-api"""

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions += "-deprecation"

resolvers ++= Seq(
	"Typesafe releases repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Typesafe snaphots repository" at "http://repo.typesafe.com/typesafe/snapshots/"
)

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.1.6" % "test",
	"com.typesafe.akka" %% "akka-actor" % "2.3.3",
	"org.reactivemongo" %% "reactivemongo" % "0.11.0-SNAPSHOT"
)
