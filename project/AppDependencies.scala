import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object AppDependencies {

  private val typesafe = "com.typesafe.play"
  private val hmrc = "uk.gov.hmrc"

  val compile: Seq[ModuleID] = Seq(
    ws,
    hmrc                %% "bootstrap-play-26"        % "1.3.0",
    "com.github.fge"    % "json-schema-validator"     % "2.2.6",
    typesafe            %% "play-json-joda"           % "2.8.1"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play"    %% "scalatestplus-play"   % "3.1.3"                 % scope,
        "org.scalatest"             %% "scalatest"            % "3.0.8"                 % scope,
        hmrc                        %% "hmrctest"             % "3.9.0-play-26"         % scope,
        "org.pegdown"               % "pegdown"               % "1.6.0"                 % scope,
        "org.mockito"               % "mockito-core"          % "3.2.4"                 % scope,
        typesafe                    %% "play-test"            % PlayVersion.current     % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}