import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-eligibility"
  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  private val typesafe = "com.typesafe.play"
  private val hmrc = "uk.gov.hmrc"

  val compile: Seq[ModuleID] = Seq(
    ws,
    hmrc %% "bootstrap-play-26" % "0.38.0",
    hmrc %% "govuk-template" % "5.31.0-play-26",
    hmrc %% "play-ui" % "7.38.0-play-26",
    "com.github.fge" % "json-schema-validator" % "2.2.6",
    "com.kenshoo" %% "metrics-play" % "2.3.0_0.2.1",
    typesafe %% "play-json" % "2.6.13",
    typesafe %% "play-json-joda" % "2.6.13"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
        "org.scalatest" %% "scalatest" % "3.0.7" % scope,
        hmrc %% "hmrctest" % "3.6.0-play-26" % scope,
        "org.pegdown" % "pegdown" % "1.6.0" % scope,
        "org.mockito" % "mockito-core" % "2.27.0" % scope,
        typesafe %% "play-test" % PlayVersion.current % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}