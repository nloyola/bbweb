import com.typesafe.config._
import java.nio.file.{Files, StandardCopyOption}

val conf = ConfigFactory.parseFile(new File("conf/application.conf")).resolve()

name := "bbweb"

version := conf.getString("app.version")

maintainer in Linux := "Canadian BioSample Repository <tech@biosample.ca>"

packageSummary in Linux := "Biorepository application for tracking biospecimens."

packageDescription := "Biorepository application for tracking biospecimens."

scalaVersion := "2.12.10"

val akkaVer           = "2.5.23"
val silhouetteVersion = "6.1.1"

organization in ThisBuild := "org.biobank"

lazy val copyLogbackTest       = taskKey[Unit]("copyLogbackTest")
lazy val forcedCopyLogbackTest = taskKey[Unit]("forcedCopyLogbackTest")
lazy val copyTestData          = taskKey[Unit]("copyTestData")
lazy val copyEmailConf         = taskKey[Unit]("copyEmailConf")
lazy val developmentInit       = taskKey[Unit]("developmentInit")

//val sourceRoot = new File(".").getAbsoluteFile.getParent

def copyTemplate(templateName: String, destName: String): Unit = {
  val template = new File(templateName)
  val dest     = new File(destName)
  if (dest.exists) {
    println(s"file $destName already exists")
  } else {
    Files.copy(template.toPath, dest.toPath)
    println(s"file $templateName copied to $destName")
  }
}

def forcedCopyTemplate(templateName: String, destName: String): Unit = {
  val template = new File(templateName)
  val dest     = new File(destName)
  Files.copy(template.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
  println(s"file $destName overwritten with $templateName")
}

lazy val root = (project in file("."))
  .enablePlugins(PlayScala, DebianPlugin)
  .settings(
    copyLogbackTest := {
      copyTemplate("conf/logback-test.xml.template", "conf/logback-test.xml")
    },
    forcedCopyLogbackTest := {
      forcedCopyTemplate("conf/logback-test.xml.template", "conf/logback-test.xml")
    },
    copyTestData := {
      copyTemplate("conf/testdata.conf.template", "conf/testdata.conf")
    },
    copyEmailConf := {
      copyTemplate("conf/email.conf.template", "conf/email.conf")
    },
    developmentInit := {
      copyLogbackTest.value
      copyTestData.value
      copyEmailConf.value
    }
  )

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report")

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-oDS")

fork in Test := true

javaOptions in Test ++= Seq("-Xms512M",
                            "-Xmx2G",
                            "-XX:+CMSClassUnloadingEnabled",
                            "-Dconfig.file=conf/test.conf",
                            "-Dlogger.resource=logback-test.xml")

javacOptions in ThisBuild ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

javaOptions in run ++= Seq("-Xms256M", "-Xmx2G", "-XX:+UseConcMarkSweepGC")

sources in (Compile, doc) ~= (_ filter (_.getParent contains "org/biobank"))

fork in run := true

run / javaOptions += "-Xmx2G"
run / javaOptions += "-Duser.timezone=GMT"

// https://scalameta.org/metals/docs/build-tools/sbt.html
addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.2")
//addCompilerPlugin(MetalsPlugin.semanticdbModule) // enable SemanticDB

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

libraryDependencies += guice
libraryDependencies += ws
libraryDependencies += ehcache
libraryDependencies += filters
libraryDependencies += "org.scala-stm"     %% "scala-stm"             % "0.9.1"
libraryDependencies += "com.iheart"        %% "ficus"                 % "1.4.7"
libraryDependencies += "com.typesafe.play" %% "play-json"             % "2.7.4"
libraryDependencies += "com.typesafe.play" %% "play-slick"            % "4.0.2"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "4.0.2"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence"      % akkaVer % "compile" excludeAll (ExclusionRule(
  organization = "com.google.protobuf"
))
libraryDependencies += "com.typesafe.akka"   %% "akka-persistence-query" % akkaVer % "compile"
libraryDependencies += "com.typesafe.akka"   %% "akka-remote"            % akkaVer % "compile"
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-jdbc"  % "3.5.2" % "compile" excludeAll (ExclusionRule(
  organization = "com.typesafe.akka"
))
libraryDependencies += "mysql"                      % "mysql-connector-java"             % "8.0.18"
libraryDependencies += "org.scalaz"                 %% "scalaz-core"                     % "7.2.29" % "compile"
libraryDependencies += "com.github.mauricio"        %% "mysql-async"                     % "0.2.21"
libraryDependencies += "com.github.t3hnar"          %% "scala-bcrypt"                    % "4.1"
libraryDependencies += "com.github.ancane"          %% "hashids-scala"                   % "1.3"
libraryDependencies += "com.typesafe.play"          %% "play-mailer"                     % "7.0.1"
libraryDependencies += "com.typesafe.play"          %% "play-mailer-guice"               % "7.0.1"
libraryDependencies += "net.codingwell"             %% "scala-guice"                     % "4.2.6"
libraryDependencies += "com.mohiva"                 %% "play-silhouette"                 % silhouetteVersion % "compile"
libraryDependencies += "com.mohiva"                 %% "play-silhouette-password-bcrypt" % silhouetteVersion % "compile"
libraryDependencies += "com.mohiva"                 %% "play-silhouette-crypto-jca"      % silhouetteVersion % "compile"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"                   % "3.9.2"
libraryDependencies += "com.github.ghik"            %% "silencer-lib"                    % "1.4.2" % "compile"
libraryDependencies += "com.chuusai"                %% "shapeless"                       % "2.3.3" % "compile"

// Testing
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.2" % "test" excludeAll (ExclusionRule(
  organization = "com.typesafe.akka"
))
libraryDependencies += "com.typesafe.akka"      %% "akka-testkit"            % akkaVer           % "test"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play"      % "4.0.3"           % "test"
libraryDependencies += "com.mohiva"             %% "play-silhouette-testkit" % silhouetteVersion % "test"
libraryDependencies += "org.pegdown"            % "pegdown"                  % "1.6.0"           % "test"
libraryDependencies += "org.codehaus.janino"    % "janino"                   % "3.1.0"           % "test"
libraryDependencies += "org.mockito"            % "mockito-core"             % "3.1.0"           % "test"
libraryDependencies += "it.bitbl"               %% "scala-faker"             % "0.4"             % "test"
libraryDependencies += "org.gnieh"              %% "diffson-play-json"       % "4.0.0"           % "test"

routesGenerator := InjectedRoutesGenerator

PB.targets in Compile := Seq(scalapb.gen() -> (sourceManaged in Compile).value)

coverageExcludedPackages := "<empty>;router.*;views.html.*;Reverse.*;org.biobank.infrastructure.event.*;org.biobank.TestData"

wartremoverErrors in (Compile, compile) ++= Warts
  .allBut(Wart.ArrayEquals, Wart.Nothing, Wart.Equals, Wart.ToString)

wartremoverExcluded ++= Seq(sourceManaged.value, crossTarget.value / "routes" / "main")

scalacOptions ++=
  Seq("-target:jvm-1.8",
      "-encoding",
      "UTF-8",
      "-deprecation", // warning and location for usages of deprecated APIs
      "-feature", // warning and location for usages of features that should be imported explicitly
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-language:postfixOps",
      "-unchecked", // additional warnings where generated code depends on assumptions
      "-Xlint:_",
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused-import",
      "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
      "-Yrangepos",
      //s"-P:semanticdb:sourceroot:${sourceRoot}",
      "-P:silencer:pathFilters=main/controllers/ReverseRoutes;main/controllers/javascript/JavaScriptReverseRoutes;main/router/Routes")
