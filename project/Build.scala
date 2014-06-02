import sbt._

object OmnitoolBuild extends Build {
  lazy val root =
    Project(id = "omnitool", base = file("."))
      .aggregate(core, parser)

  lazy val core = Project("omnitool-core", base = file("core"))

  lazy val parser = Project("omnitool-parser", base = file("parser")).dependsOn(core)
}
