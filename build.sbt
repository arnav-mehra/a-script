ThisBuild / scalaVersion := "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

unmanagedSourceDirectories in Compile += file("/src")
unmanagedSourceDirectories in Compile += file("/src/parsers")
unmanagedSourceDirectories in Compile += file("/src/types")
unmanagedSourceDirectories in Compile += file("/src/pipeline")