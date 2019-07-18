import com.typesafe.config._
import java.nio.file.{Files, StandardCopyOption}

val conf = ConfigFactory.parseFile(new File("conf/application.conf")).resolve()

version := conf.getString("app.version")

val akkaVer = "2.5.23"
val silhouetteVersion = "6.1.0"

name := "bbweb"

organization in ThisBuild := "org.biobank"

def excludeSpecs2(module: ModuleID): ModuleID =
  module.excludeAll(ExclusionRule(organization = "org.specs2", name = "specs2"))
    .exclude("com.novocode", "junit-interface")

lazy val copyLogbackTest = taskKey[Unit]("copyLogbackTest")
lazy val forcedCopyLogbackTest = taskKey[Unit]("forcedCopyLogbackTest")
lazy val copyTestData = taskKey[Unit]("copyTestData")
lazy val copyEmailConf = taskKey[Unit]("copyEmailConf")
lazy val developmentInit = taskKey[Unit]("developmentInit")

def copyTemplate(templateName: String, destName: String): Unit = {
  val template = new File(templateName)
  val dest = new File(destName)
  if (dest.exists) {
    println(s"file $destName already exists")
  } else {
    Files.copy(template.toPath, dest.toPath)
    println(s"file $templateName copied to $destName")
  }
}

def forcedCopyTemplate(templateName: String, destName: String): Unit = {
  val template = new File(templateName)
  val dest = new File(destName)
  Files.copy(template.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
  println(s"file $destName overwritten with $templateName")
}

lazy val root = (project in file("."))
  .enablePlugins(PlayScala, DebianPlugin)
  .settings(
    libraryDependencies ~= (_.map(excludeSpecs2)),

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

maintainer in Linux := "Canadian BioSample Repository <tech@biosample.ca>"

packageSummary in Linux := "Biorepository application for tracking biospecimens."

packageDescription := "Biorepository application for tracking biospecimens."

scalaVersion := Option(System.getProperty("scala.version")).getOrElse("2.12.8")

scalacOptions in Compile ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-deprecation",       // warning and location for usages of deprecated APIs
    "-feature",           // warning and location for usages of features that should be imported explicitly
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-unchecked",          // additional warnings where generated code depends on assumptions
    "-Xlint:_",
    "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused
  )

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

fork in Test := true

javaOptions in Test ++=  Seq(
    "-Xms512M",
    "-Xmx2048M",
    "-XX:+CMSClassUnloadingEnabled",
    "-Dconfig.file=conf/test.conf",
    "-Dlogger.resource=logback-test.xml"
  )

javacOptions in ThisBuild  ++= Seq(
    "-source", "1.8",
    "-target", "1.8",
    "-Xlint"
  )

javaOptions in run ++= Seq(
    "-Xms256M", "-Xmx2G", "-XX:+UseConcMarkSweepGC")

sources in (Compile, doc) ~= (_ filter (_.getParent contains "org/biobank"))

fork in run := true

run / javaOptions += "-Xmx2G -Duser.timezone=GMT"

testOptions in Test := Nil

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report")

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-oDS")

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.1")

resolvers ++= Seq(
    Classpaths.sbtPluginReleases,
    "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype OSS"        at "https://oss.sonatype.org/content/repositories/releases",
    "Akka Snapshots"      at "http://repo.akka.io/snapshots/",
    Resolver.jcenterRepo
  )

libraryDependencies ++= Seq(
    guice,
    ws,
    ehcache,
    filters,
    "org.scala-stm"               %% "scala-stm"                           % "0.9.1",
    "com.iheart"                  %% "ficus"                               % "1.4.7",
    "com.typesafe.play"           %% "play-json"                           % "2.7.4",
    ( "com.typesafe.akka"         %% "akka-persistence"                    % akkaVer   % "compile"  )
      .excludeAll(ExclusionRule(organization="com.google.protobuf")),
    "com.typesafe.akka"           %% "akka-persistence-query"              % akkaVer   % "compile",
    "com.typesafe.akka"           %% "akka-remote"                         % akkaVer   % "compile",
    ( "com.github.dnvriend"       %% "akka-persistence-jdbc"               % "3.5.2"   % "compile"  )
      .excludeAll(ExclusionRule(organization="com.typesafe.akka")),
    "mysql"                       % "mysql-connector-java"                 % "8.0.16",
    "org.scalaz"                  %% "scalaz-core"                         % "7.2.28"  % "compile",
    "com.github.mauricio"         %% "mysql-async"                         % "0.2.21",
    "com.github.t3hnar"           %% "scala-bcrypt"                        % "4.1",
    "com.github.ancane"           %% "hashids-scala"                       % "1.3",
    "com.typesafe.play"           %% "play-mailer"                         % "7.0.1",
    "com.typesafe.play"           %% "play-mailer-guice"                   % "7.0.1",
    "net.codingwell"              %% "scala-guice"                         % "4.2.6",
    "com.mohiva"                  %% "play-silhouette"                     % silhouetteVersion,
    "com.mohiva"                  %% "play-silhouette-password-bcrypt"     % silhouetteVersion,
    "com.mohiva"                  %% "play-silhouette-crypto-jca"          % silhouetteVersion,
    "com.typesafe.scala-logging"  %% "scala-logging"                       % "3.9.2",
    "com.github.ghik"             %% "silencer-lib"                        % "1.4.1"   % "compile",
    "com.chuusai"                 %% "shapeless"                           % "2.3.3"   % "compile",
    // Testing
    ( "com.github.dnvriend"       %% "akka-persistence-inmemory"           % "2.5.15.2"  % "test" )
      .excludeAll(ExclusionRule(organization="com.typesafe.akka")),
    "com.typesafe.akka"           %% "akka-testkit"                        % akkaVer   % "test",
    "org.scalatestplus.play"      %% "scalatestplus-play"                  % "4.0.3"   % "test",
    "com.mohiva"                  %% "play-silhouette-testkit"             % silhouetteVersion % "test",
    "org.pegdown"                 %  "pegdown"                             % "1.6.0"   % "test",
    "org.codehaus.janino"         %  "janino"                              % "3.0.14"  % "test",
    "org.mockito"                 %  "mockito-core"                        % "3.0.0"   % "test",
    "it.bitbl"                    %% "scala-faker"                         % "0.4"     % "test",
    "org.gnieh"                   %% "diffson-play-json"                   % "3.1.1"   % "test"
  )

routesGenerator := InjectedRoutesGenerator

// To completely override the optimization process, use this config option:
//requireNativePath := Some("node r.js -o name=main out=javascript-min/main.min.js")

PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  )

coverageExcludedPackages := "<empty>;router.*;views.html.*;Reverse.*;org.biobank.infrastructure.event.*;org.biobank.TestData"

wartremoverErrors in (Compile, compile) ++= Warts.allBut(Wart.ArrayEquals, Wart.Nothing, Wart.Equals, Wart.ToString)

wartremoverExcluded ++= Seq(sourceManaged.value, crossTarget.value / "routes" / "main")
