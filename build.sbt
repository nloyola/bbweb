import com.typesafe.config._
import java.nio.file.{Files, StandardCopyOption}
import play.sbt.routes.RoutesKeys

val conf = ConfigFactory.parseFile(new File("conf/application.conf")).resolve()

name := "bbweb"

version := conf.getString("app.version")

maintainer in Linux := "Canadian BioSample Repository <tech@biosample.ca>"

packageSummary in Linux := "Biorepository application for tracking biospecimens."

packageDescription := "Biorepository application for tracking biospecimens."

scalaVersion := "2.13.1"

val akkaVer           = "2.6.1"
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

// disable following line for now since it causes a compilation error when generating
// code coverage report
// (testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report")

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-oDS")

fork in Test := true

javaOptions in Test ++= Seq("-Xms512M",
                            "-Xmx2G",
                            "-XX:+CMSClassUnloadingEnabled",
                            "-Dconfig.file=conf/test.conf",
                            "-Dlogger.resource=logback-test.xml")

javacOptions in ThisBuild ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

javaOptions in run ++= Seq("-Xms256M", "-Xmx2G")

sources in (Compile, doc) ~= (_ filter (_.getParent contains "org/biobank"))

fork in run := true

// https://github.com/playframework/playframework/issues/7382
RoutesKeys.routesImport -= "controllers.Assets.Asset"

run / javaOptions += "-Xmx2G"
run / javaOptions += "-Duser.timezone=GMT"

// https://scalameta.org/metals/docs/build-tools/sbt.html
//addCompilerPlugin(MetalsPlugin.semanticdbModule) // enable SemanticDB

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

libraryDependencies += guice
libraryDependencies += ws
libraryDependencies += ehcache
libraryDependencies += filters
libraryDependencies += "org.scala-stm"     %% "scala-stm"             % "0.9.1"
libraryDependencies += "com.iheart"        %% "ficus"                 % "1.4.7"
libraryDependencies += "com.typesafe.play" %% "play-json"             % "2.8.1"
libraryDependencies += "com.typesafe.play" %% "play-slick"            % "5.0.0"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "5.0.0"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence"      % akkaVer % "compile" excludeAll (ExclusionRule(
  organization = "com.google.protobuf"
))
libraryDependencies += "com.typesafe.akka"   %% "akka-persistence-query" % akkaVer % "compile"
libraryDependencies += "com.typesafe.akka"   %% "akka-remote"            % akkaVer % "compile"
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-jdbc"  % "3.5.2" % "compile" excludeAll (ExclusionRule(
  organization = "com.typesafe.akka"
))
libraryDependencies += "mysql"                      % "mysql-connector-java"             % "8.0.19"
libraryDependencies += "org.scalaz"                 %% "scalaz-core"                     % "7.2.30" % "compile"
libraryDependencies += "com.github.t3hnar"          %% "scala-bcrypt"                    % "4.1"
libraryDependencies += "com.typesafe.play"          %% "play-mailer"                     % "7.0.1"
libraryDependencies += "com.typesafe.play"          %% "play-mailer-guice"               % "7.0.1"
libraryDependencies += "net.codingwell"             %% "scala-guice"                     % "4.2.6"
libraryDependencies += "com.mohiva"                 %% "play-silhouette"                 % silhouetteVersion % "compile"
libraryDependencies += "com.mohiva"                 %% "play-silhouette-password-bcrypt" % silhouetteVersion % "compile"
libraryDependencies += "com.mohiva"                 %% "play-silhouette-crypto-jca"      % silhouetteVersion % "compile"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"                   % "3.9.2"
libraryDependencies += "com.chuusai"                %% "shapeless"                       % "2.3.3" % "compile"
libraryDependencies += "io.underscore"              %% "slickless"                       % "0.3.6"

// Testing
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.2" % "test" excludeAll (ExclusionRule(
  organization = "com.typesafe.akka"
))
libraryDependencies += "com.typesafe.akka"      %% "akka-testkit"            % akkaVer % "test"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play"      % "5.0.0" % "test"
libraryDependencies += "com.mohiva"             %% "play-silhouette-testkit" % silhouetteVersion % "test"
libraryDependencies += "org.pegdown"            % "pegdown"                  % "1.6.0" % "test"
libraryDependencies += "org.codehaus.janino"    % "janino"                   % "3.1.0" % "test"
libraryDependencies += "org.mockito"            % "mockito-core"             % "3.2.4" % "test"
libraryDependencies += "org.gnieh"              %% "diffson-play-json"       % "4.0.1" % "test"
libraryDependencies += "com.h2database"         % "h2"                       % "1.4.200" % "test"
libraryDependencies += "com.github.pjfanning"   %% "scala-faker"             % "0.5.0"

routesGenerator := InjectedRoutesGenerator

PB.targets in Compile := Seq(scalapb.gen() -> (sourceManaged in Compile).value)

coverageExcludedPackages := "<empty>;router.*;views.html.*;Reverse.*;org.biobank.infrastructure.event.*;org.biobank.TestData"

//compileOrder := CompileOrder.Mixed

wartremoverErrors in (Compile, compile) ++= Warts
  .allBut(Wart.ArrayEquals, Wart.Nothing, Wart.Equals, Wart.ToString, Wart.StringPlusAny)

wartremoverExcluded ++= Seq(sourceManaged.value, crossTarget.value / "routes" / "main")

scalacOptions += "-target:jvm-1.8"
scalacOptions += "-encoding"
scalacOptions += "UTF-8"
//scalacOptions += "-deprecation" // warning and location for usages of deprecated APIs
scalacOptions += "-feature" // warning and location for usages of features that should be imported explicitly
scalacOptions += "-language:implicitConversions"
scalacOptions += "-language:higherKinds"
scalacOptions += "-language:existentials"
scalacOptions += "-language:postfixOps"
scalacOptions += "-unchecked" // additional warnings where generated code depends on assumptions
scalacOptions += "-Ywarn-dead-code"
scalacOptions += "-Ywarn-numeric-widen"
scalacOptions += "-Wunused:imports"
scalacOptions += "-Ywarn-value-discard" // Warn when non-Unit expression results are unused
scalacOptions += "-Yrangepos"
scalacOptions += "-Xlint:_"
scalacOptions += "-Xlint:adapted-args" // Warn if an argument list is modified to match the receiver
scalacOptions += "-Xlint:inaccessible"
scalacOptions += "-Xsource:2.13"
//scalacOptions += "-Xprint:typer"
