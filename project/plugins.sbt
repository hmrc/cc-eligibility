resolvers += Resolver.url("hmrc-sbt-plugin-releases",
  url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "scoverage-bintray" at "https://dl.bintray.com/sksamuel/sbt-plugins/"

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "1.4.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "0.9.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "0.12.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.10")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
