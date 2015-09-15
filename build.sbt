name := """api-doc"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  specs2 % Test
)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
