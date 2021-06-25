import uk.gov.hmrc.DefaultBuildSettings.targetJvm

lazy val appName = "cc-eligibility"
lazy val appDependencies : Seq[ModuleID] = ???
lazy val plugins : Seq[Plugins] = Seq(
  SbtDistributablesPlugin
)
lazy val playSettings : Seq[Setting[_]] = Seq.empty

lazy val scoverageSettings = {
  import scoverage._
  Seq(
    ScoverageKeys.coverageExcludedPackages :=  "<empty>;Reverse.*;.*Routes.*;routes_routing.*;uk.gov.hmrc;config.*;",
    ScoverageKeys.coverageMinimumStmtTotal := 95,
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
  .settings(
    scalacOptions += "-P:silencer:pathFilters=routes",
    scalacOptions += "-P:silencer:lineContentFilters=^\\w",
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.1" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.1" % Provided cross CrossVersion.full
    )
  )
  .settings(
    targetJvm := "jvm-1.8",
    scalaVersion := "2.12.12",
    libraryDependencies ++= AppDependencies(),
    retrieveManaged := true,
    evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false)
  )
  .settings(
    isPublicArtefact := true
  )
