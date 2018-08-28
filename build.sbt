name := "delphi-crawler"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.4"

lazy val crawler = (project in file(".")).
  enablePlugins(JavaAppPackaging).
  enablePlugins(DockerPlugin).
  settings (
    dockerBaseImage := "openjdk:jre-alpine"
  ).
  enablePlugins(AshScriptPlugin).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "de.upb.cs.swt.delphi.crawler"
  )

scalastyleConfig := baseDirectory.value / "project" / "scalastyle_config.xml"


mainClass in (Compile, run) := Some("de.upb.cs.swt.delphi.crawler.Crawler")
mainClass in (Compile, packageBin) := Some("de.upb.cs.swt.delphi.crawler.Crawler")
mainClass in Compile :=  Some("de.upb.cs.swt.delphi.crawler.Crawler")

val akkaVersion = "2.5.14"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.8",
  "org.json4s" %% "json4s-jackson" % "3.5.3",
  "io.swagger" % "swagger-core" % "1.5.15",
  "io.spray" % "spray-client" % "1.3.1"
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime

val elastic4sVersion = "6.3.0"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion,

  // for the http client
  "com.sksamuel.elastic4s" %% "elastic4s-http" % elastic4sVersion,

  // if you want to use reactive streams
  "com.sksamuel.elastic4s" %% "elastic4s-http-streams" % elastic4sVersion,

  // testing
  "com.sksamuel.elastic4s" %% "elastic4s-testkit" % elastic4sVersion % "test",
  "com.sksamuel.elastic4s" %% "elastic4s-embedded" % elastic4sVersion % "test"
)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers ++= Seq(Resolver.mavenLocal)

val opalVersion = "1.0.0"
libraryDependencies ++= Seq(
  "de.opal-project" % "common_2.12" % opalVersion,
  "de.opal-project" % "opal-developer-tools_2.12" % opalVersion
)

val mavenVersion = "3.5.2"
libraryDependencies ++= Seq (
  "org.apache.maven" % "maven-core" % mavenVersion,
  "org.apache.maven" % "maven-model" % mavenVersion,
  "org.apache.maven" % "maven-repository-metadata" % mavenVersion,
  "org.apache.maven" % "maven-resolver-provider" % mavenVersion
)

libraryDependencies ++= Seq(
  "io.get-coursier" %% "coursier" % "1.0.1",
  "io.get-coursier" %% "coursier-cache" % "1.0.1"
)

libraryDependencies += "org.apache.maven.indexer" % "indexer-reader" % "6.0.0"
libraryDependencies += "org.apache.maven.indexer" % "indexer-core" % "6.0.0"

// Pinning secure versions of insecure transitive libraryDependencies
// Please update when updating dependencies above (including Play plugin)
libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "25.1-jre"
)
