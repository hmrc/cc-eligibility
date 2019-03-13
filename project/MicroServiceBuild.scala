import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-eligibility"
  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  private val bootstrapPlay25Version = "4.9.0"
  private val playUiVersion = "7.33.0-play-25"
  private val govUkTemplateVersion = "5.30.0-play-25"
  private val jsonSchemaValidator = "2.2.6"

  private val scalaTestVersion = "3.0.0"
  private val scalaTestPlusVersion = "2.0.0"
  private val pegDownVersion = "1.6.0"
  private val hmrcTestVersion = "3.6.0-play-25"
  private val mockitoVersion = "2.18.3"


  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "bootstrap-play-25" % bootstrapPlay25Version,
    "uk.gov.hmrc" %% "govuk-template" % govUkTemplateVersion,
    "uk.gov.hmrc" %% "play-ui" % playUiVersion,
    "com.github.fge" % "json-schema-validator" % jsonSchemaValidator,
    "com.kenshoo" %% "metrics-play" % "2.3.0_0.1.8"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply() = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play" %% "scalatestplus-play" % scalaTestPlusVersion % scope,
        "org.scalatest" %% "scalatest" % scalaTestVersion % scope,
        "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
        "org.pegdown" % "pegdown" % pegDownVersion % scope,
        "org.mockito" % "mockito-core" % mockitoVersion % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}