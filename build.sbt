import Dependencies._

lazy val root = (project in file("."))
  .settings(
    name := "PSO-AGP",
    scalaVersion := "2.12.1",
    libraryDependencies += scalaTest % Test,
    retrieveManaged := true,
    assemblyJarName in assembly := "AGP.jar"
  )
