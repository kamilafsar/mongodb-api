// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq( "-unchecked", "-deprecation" )

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")
