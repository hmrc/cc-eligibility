import uk.gov.hmrc.DefaultBuildSettings.targetJvm
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

lazy val appName = "cc-eligibility"
lazy val appDependencies : Seq[ModuleID] = ???
lazy val plugins : Seq[Plugins] = Seq(
  SbtAutoBuildPlugin,
  SbtGitVersioning,
  SbtDistributablesPlugin,
  SbtArtifactory
)
lazy val playSettings : Seq[Setting[_]] = Seq.empty

lazy val scoverageSettings = {
  import scoverage._
  Seq(
    ScoverageKeys.coverageExcludedPackages :=  "<empty>;Reverse.*;.*Routes.*;routes_routing.*;uk.gov.hmrc;config.*;",
    ScoverageKeys.coverageMinimum := 95,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )
}

lazy val microservice = Project(appName, file("."))
  .enablePlugins(Seq(play.sbt.PlayScala) ++ plugins : _*)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(majorVersion := 1)
  .settings(playSettings ++ scoverageSettings : _*)
  .settings(PlayKeys.playDefaultPort := 9375)
  .settings(publishingSettings : _*)
  .settings(
    targetJvm := "jvm-1.8",
    scalaVersion := "2.12.10",
    libraryDependencies ++= AppDependencies(),
    retrieveManaged := true,
    evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
    resolvers := Seq(
      Resolver.bintrayRepo("hmrc", "releases"),
      Resolver.jcenterRepo
    )
  )