import sbt._
import play.PlayImport._
import play.core.PlayVersion

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-eligibility"

  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  private val microserviceBootstrapVersion = "4.4.0"
  private val playHealthVersion = "1.1.0"
  private val jsonSchemaValidator = "2.2.6"
  private val playConfigVersion = "3.0.0"
  private val playAuthorisationVersion = "3.4.0"
  private val playJsonLoggerVersion = "2.1.1"
  private val scalaTestVersion = "2.2.6"
  private val scalaTestPlusVersion = "1.2.0"
  private val pegDownVersion = "1.6.0"
  private val hmrcTestVersion = "1.9.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microserviceBootstrapVersion,
    "uk.gov.hmrc" %% "play-config" % playConfigVersion,
    "uk.gov.hmrc" %% "play-json-logger" % playJsonLoggerVersion,
    "uk.gov.hmrc" %% "play-authorisation" % playAuthorisationVersion,
    "uk.gov.hmrc" %% "play-health" % playHealthVersion,
    "com.github.fge" % "json-schema-validator" % jsonSchemaValidator,
    "com.kenshoo" %% "metrics-play" % "2.3.0_0.1.8",
    "com.codahale.metrics" % "metrics-graphite" % "3.0.2"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply() = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus" %% "play" % scalaTestPlusVersion % scope,
        "org.scalatest" %% "scalatest" % scalaTestVersion % scope,
        "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
        "org.pegdown" % "pegdown" % pegDownVersion % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}