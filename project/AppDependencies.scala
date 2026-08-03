import play.core.PlayVersion
import play.sbt.PlayImport.*
import sbt.*

object AppDependencies {

  private val bootstrapVersion = "10.7.0"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"               %% "bootstrap-backend-play-30" % bootstrapVersion
  )

  trait TestDependencies {
    lazy val scope: String       = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {

    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "uk.gov.hmrc"       %% "bootstrap-test-play-30" % bootstrapVersion    % scope,
        "org.scalatestplus" %% "mockito-5-12"  % "3.2.19.0"          % scope,
        "org.playframework" %% "play-test"              % PlayVersion.current % scope
      )
    }.test

  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}
