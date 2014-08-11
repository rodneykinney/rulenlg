resolvers += Resolver.url(
  "allenai-bintray-sbt-plugins",
  url("http://dl.bintray.com/content/allenai/sbt-plugins"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-deploy" % "2014.07.03-0")

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-format" % "2014.07.03-0")

