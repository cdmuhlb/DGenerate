name := "ScalaSpEC-Harvest"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.0"

libraryDependencies += "com.typesafe.akka" %% "akka-cluster" % "2.2.0"

libraryDependencies += "org.scalanlp" % "breeze-math_2.10" % "0.3"

scalacOptions ++= Seq("-target:jvm-1.7", "-deprecation", "-feature", "-unchecked")
