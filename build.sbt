name := "mustached-octo-hipster"

version := "1.0"

scalaVersion := "2.10.1"

net.virtualvoid.sbt.graph.Plugin.graphSettings

resolvers += "Eligosource Releases" at "http://repo.eligotech.com/nexus/content/repositories/eligosource-releases"

libraryDependencies += "org.eligosource" %% "eventsourced-core" % "0.6.0"

libraryDependencies += "org.eligosource" %% "eventsourced-journal-leveldb" % "0.6.0"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.9.13"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M8"

libraryDependencies += "org.specs2" % "specs2_2.10" % "1.13"

