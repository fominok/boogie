name := "BoogieBot"

fork in run := true

version := "4"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "1.1.5",
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4-M3",
  "org.mongodb.scala" % "mongo-scala-driver_2.11" % "1.0.0"
)
