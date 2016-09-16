import sbt.Keys._
//import sbt.Tests.{Group, SubProcess}
import sbt._
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import uk.gov.hmrc.versioning.SbtGitVersioning

trait MicroService {

//  import TestPhases._
//  import uk.gov.hmrc.DefaultBuildSettings._

  val appName: String

  lazy val appDependencies : Seq[ModuleID] = ???
  lazy val plugins : Seq[Plugins] = Seq()
  lazy val playSettings : Seq[Setting[_]] = Seq.empty

  lazy val scoverageSettings = {
    import scoverage._
    Seq(
      ScoverageKeys.coverageExcludedPackages :=  "<empty>;Reverse.*;.*Routes.*;routes_routing.*;uk.gov.hmrc;.*EnumUtils.*;models.output.*;config.*;",
      ScoverageKeys.coverageMinimum := 95,
      ScoverageKeys.coverageFailOnMinimum := true,
      ScoverageKeys.coverageHighlighting := true
    )
  }

  lazy val microservice = Project(appName, file("."))
    .enablePlugins(Seq(play.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin) ++ plugins : _*)
    .settings(playSettings ++ scoverageSettings : _*)
    .settings(publishingSettings : _*)
    .settings(
//      targetJvm := "jvm-1.8",
//      parallelExecution in Test := false,
//      fork in Test := false,
//      ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
    )
//    .settings(inConfig(TemplateTest)(Defaults.testSettings): _*)
//    .configs(IntegrationTest)
//    .settings(inConfig(TemplateItTest)(Defaults.itSettings): _*)
    .settings(
      libraryDependencies ++= appDependencies,
      retrieveManaged := true,
//      Keys.fork in IntegrationTest := false,
//      unmanagedSourceDirectories in IntegrationTest <<= (baseDirectory in IntegrationTest)(base => Seq(base / "it")),
//      addTestReportOption(IntegrationTest, "int-test-reports"),
//      testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
//      parallelExecution in IntegrationTest := false,
      resolvers := Seq(
//        Resolver.bintrayRepo("hmrc", "releases"),
        Resolver.typesafeRepo("releases")
      )
    )
}

/*private object TestPhases {

  val allPhases = "tt->test;test->test;test->compile;compile->compile"
  val allItPhases = "tit->it;it->it;it->compile;compile->compile"

  lazy val TemplateTest = config("tt") extend Test
  lazy val TemplateItTest = config("tit") extend IntegrationTest

  def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
    tests map {
      test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
    }
}*/


