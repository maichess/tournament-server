ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.3"

val zioVersion     = "2.1.16"
val zioHttpVersion = "3.2.0"
val zioJsonVersion = "0.7.39"

lazy val root = (project in file("."))
  .settings(
    name := "tournament-server",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"              % zioVersion,
      "dev.zio" %% "zio-streams"      % zioVersion,
      "dev.zio" %% "zio-http"         % zioHttpVersion,
      "dev.zio" %% "zio-json"         % zioJsonVersion,
      "dev.zio" %% "zio-test"         % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt"     % zioVersion % Test,
      "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    coverageMinimumStmtTotal := 100,
    coverageFailOnMinimum    := true,
    coverageExcludedFiles    := ".*Main.*",

    assembly / assemblyJarName := "app.jar",
    assembly / test            := {},
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
      case PathList("META-INF", "native-image", xs @ _*) => MergeStrategy.discard
      case PathList("META-INF", _*)                  => MergeStrategy.discard
      case PathList("module-info.class")             => MergeStrategy.discard
      case "reference.conf"                          => MergeStrategy.concat
      case _                                         => MergeStrategy.first
    },
  )
