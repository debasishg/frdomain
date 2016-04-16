import sbt._
import Keys._

object FRDomainProject extends Build
{
  import Resolvers._

  resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
  
  lazy val root = Project("FRDomain", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    version := "0.01",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.11.7"),

    scalacOptions in Compile ++= Seq( "-unchecked", "-feature", "-language:postfixOps", "-deprecation" )
  )

  val akkaVersion = "2.3.11"

  lazy val coreSettings = commonSettings ++ Seq(
    name := "FRDomain",
    libraryDependencies := Seq(
      "org.scalaz"                   %% "scalaz-core"                   % "7.2.2",
      "org.scalaz"                   %% "scalaz-concurrent"             % "7.2.2",
      "joda-time"                    %  "joda-time"                     % "2.9.1",
      "org.joda"                     %  "joda-convert"                  % "1.8.1",
      "io.spray"                     %% "spray-json"                    % "1.3.2",
      "com.typesafe.akka"            %% "akka-actor"                    % akkaVersion,
      "com.typesafe.akka"            %% "akka-persistence-experimental" % akkaVersion,
      "com.typesafe.akka"            %  "akka-stream-experimental_2.11" % "1.0-RC4",
      "com.typesafe.scala-logging"   %% "scala-logging-slf4j"           % "2.1.2",
      "com.typesafe.slick"           %% "slick"                         % "3.0.0",
      "com.h2database"                % "h2"                            % "1.4.187",
      "com.zaxxer"                    % "HikariCP-java6"                % "2.3.8",
      "ch.qos.logback"               %  "logback-classic"               % "1.1.3",
      "org.scalacheck"               %%  "scalacheck"                   % "1.12.5"       % "test"
    ),
    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype.credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/frdomain</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/frdomain.git</url>
        <connection>scm:git:git@github.com:debasishg/frdomain.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )
}
