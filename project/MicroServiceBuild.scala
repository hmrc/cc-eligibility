import play.sbt.PlayImport._
import play.core.PlayVersion
import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-eligibility"
  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  private val microserviceBootstrapVersion = "6.12.0"
//  private val playHealthVersion = "2.1.0"
  private val jsonSchemaValidator = "2.2.6"
//  private val playConfigVersion = "4.3.0"
//  private val playAuthorisationVersion = "4.3.0"
//  private val logbackJsonLoggerVersion = "3.1.0"
  private val scalaTestVersion = "2.2.6"
  private val scalaTestPlusVersion = "1.5.1"
  private val pegDownVersion = "1.6.0"
  private val hmrcTestVersion = "2.3.0"
  private val mockitoVersion = "1.10.19"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microserviceBootstrapVersion,
//    "uk.gov.hmrc" %% "play-config" % playConfigVersion,
//    "uk.gov.hmrc" %% "logback-json-logger" % logbackJsonLoggerVersion,
//    "uk.gov.hmrc" %% "play-authorisation" % playAuthorisationVersion,
//    "uk.gov.hmrc" %% "play-health" % playHealthVersion,
    "com.github.fge" % "json-schema-validator" % jsonSchemaValidator,
    "com.kenshoo" %% "metrics-play" % "2.3.0_0.1.8"
//    "com.codahale.metrics" % "metrics-graphite" % "3.0.2"
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
        "org.mockito" % "mockito-all" % mockitoVersion % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}