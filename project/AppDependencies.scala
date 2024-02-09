import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object AppDependencies {

  private val typesafe = "com.typesafe.play"
  private val hmrc = "uk.gov.hmrc"

  private val bootstrapVersion = "8.4.0"
  private val pekkoVersion = "1.0.2"


  val compile: Seq[ModuleID] = Seq(
    ws,
    hmrc                %% "bootstrap-backend-play-30" % bootstrapVersion,
    "com.github.fge"    % "json-schema-validator"      % "2.2.14",
    typesafe            %% "play-json-joda"            % "2.10.4"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "uk.gov.hmrc"               %% "bootstrap-test-play-30"     % bootstrapVersion     % scope,
        "org.scalatestplus"         %% "scalatestplus-mockito"      % "1.0.0-M2"           % scope,
        "org.pegdown"               % "pegdown"                     % "1.6.0"              % scope,
        "org.mockito"               % "mockito-core"                % "5.10.0"              % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}