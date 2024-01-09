ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"
libraryDependencies ++= Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.27.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.27.1"
)
lazy val root = (project in file("."))
  .settings(
    name := "vote-resolver"
  )
