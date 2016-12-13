name := "au-scala-fall-2016"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.jsoup" %  "jsoup" % "1.10.1",
  "org.scala-lang" % "scala-reflect" % "2.11.8",
  "org.scala-lang" % "scala-compiler" % "2.11.8"
)

scalacOptions += "-Ymacro-debug-lite"