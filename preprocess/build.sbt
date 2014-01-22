// Basic Project Settings

name := "preprocess"

version := "0.0.0"

scalaVersion := "2.10.3"


// Dependencies

libraryDependencies += "org.rogach" %% "scallop" % "0.9.4"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.0" classifier "models"
)

// Compile Options

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:reflectiveCalls"
)


// Packaging

exportJars := true

packageArchetype.java_application
