import sbt._

object Dependencies {
  val sprayJson = "io.spray" %%  "spray-json" % "1.2.6"

  val defaultResolvers = Seq(
    "spray" at "http://repo.spray.io/"
  )
}
