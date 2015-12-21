name := "Proyecto SBT"

scalaVersion := "2.11.7"

lazy val proyecto = FDProject( 
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
