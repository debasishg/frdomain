lazy val commonSettings = Seq(
  version := "0.0.1",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.12.6", "2.11.8"),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  libraryDependencies ++= Seq(
      "org.typelevel"                %% "cats-core"                     %"1.1.0",
      "org.typelevel"                %% "cats-effect"                   %"1.0.0-RC2",
      "joda-time"                     % "joda-time"                     % "2.9.1",
      "org.joda"                      % "joda-convert"                  % "1.8.1",
      "io.spray"                     %% "spray-json"                    % "1.3.2",
      "com.typesafe.scala-logging"   %% "scala-logging"                 % "3.5.0",
      "com.typesafe.slick"            % "slick_2.12.0-M5"               % "3.2.0-M1",
      "com.h2database"                % "h2"                            % "1.4.187",
      "com.zaxxer"                    % "HikariCP-java6"                % "2.3.8",
      "ch.qos.logback"                % "logback-classic"               % "1.1.7",
      "org.scalacheck"               %% "scalacheck"                    % "1.13.4"       % "test"
    )
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "frdomain",
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-language:higherKinds",
      "-language:postfixOps",
      "-deprecation"
    )
  )

