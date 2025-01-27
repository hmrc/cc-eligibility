import play.core.PlayVersion
import play.sbt.PlayImport.*
import sbt.*

object AppDependencies {

  private val bootstrapVersion = "9.7.0"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-30" % bootstrapVersion,
    "com.github.java-json-tools"   % "json-schema-validator"      % "2.2.14"
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
        "org.playframework"         %% "play-test"                  % PlayVersion.current  % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}