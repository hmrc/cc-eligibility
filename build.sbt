lazy val appName                        = "cc-eligibility"
lazy val appDependencies: Seq[ModuleID] = ???

lazy val plugins: Seq[Plugins] = Seq(
  SbtDistributablesPlugin
)

lazy val playSettings: Seq[Setting[_]] = Seq.empty

ThisBuild / majorVersion := 1
ThisBuild / scalaVersion := "3.3.7"

lazy val scoverageSettings = {
  import scoverage._
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;Reverse.*;.*Routes.*;routes_routing.*;uk.gov.hmrc;config.*;",
    ScoverageKeys.coverageMinimumStmtTotal := 95,
    ScoverageKeys.coverageFailOnMinimum    := true,
    ScoverageKeys.coverageHighlighting     := true
  )
}

lazy val microservice = Project(appName, file("."))
  .enablePlugins(Seq(play.sbt.PlayScala) ++ plugins: _*)
  .disablePlugins(JUnitXmlReportPlugin) // Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(playSettings ++ scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 9375)
  .settings(
    scalacOptions ++= Seq(
      "-Wconf:cat=unused-imports&src=.*routes.*:s",
      "-Wconf:cat=unused-privates&src=.*routes.*:s"
    )
  )
  .settings(
    libraryDependencies ++= AppDependencies(),
    retrieveManaged := true
  )
  .settings(
    isPublicArtefact := true
  )
