name := "FunctionalProgramming"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

libraryDependencies <++= scalaVersion { v =>
  Seq(
    "org.scala-lang" % "scalap" % v,
    "org.scala-lang" % "scala-compiler" % v,
    "org.scala-lang" % "scala-reflect" % v
  )
}

    