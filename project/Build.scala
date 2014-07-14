//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

import sbt._, Keys._

import sbtunidoc.Plugin._, UnidocKeys._

import au.com.cba.omnia.uniform.core.standard.StandardProjectPlugin._
import au.com.cba.omnia.uniform.dependency.UniformDependencyPlugin._

object OmnitoolBuild extends Build {
  lazy val standardSettings =
    Defaults.defaultSettings ++
    uniformDependencySettings ++
    uniform.docSettings("https://github.com/CommBank/edge")

  lazy val root =
    Project(
      id = "omnitool",
      base = file("."),
      settings =
        standardSettings ++
        uniform.project("omnitool-all", "au.com.cba.omnia.omnitool.all") ++
          uniform.ghsettings ++
        Seq(publishArtifact := false)
    )
      .aggregate(core, parser, time)

  lazy val core = Project(
    id ="omnitool-core",
    base = file("core"),
    settings =
      standardSettings ++
      uniform.project("omnitool-core", "au.com.cba.omnia.omnitool.core") ++
      Seq(
        libraryDependencies :=
          depend.scalaz() ++ depend.testing() ++
          Seq(
            "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
          )
      )
  )

  lazy val parser = Project(
    id ="omnitool-parser",
    base = file("parser"),
    settings =
      standardSettings ++
      uniform.project("omnitool-parser", "au.com.cba.omnia.omnitool.parser") ++
      Seq(
        libraryDependencies :=
          depend.scalaz() ++
          Seq(
            "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
          )
      )
  ).dependsOn(core)

  lazy val time = Project(
    id ="omnitool-time",
    base = file("time"),
    settings =
      standardSettings ++
      uniform.project("omnitool-time", "au.com.cba.omnia.omnitool.time") ++
      Seq(
        apiMappings in (ScalaUnidoc, unidoc) <++= (fullClasspath in Compile).map(cp => Seq(
          assignApiUrl(cp, "joda-time", "joda-time", "http://www.joda.org/joda-time/apidocs/")
        ).flatten.toMap),
        libraryDependencies :=
          depend.time() ++
          depend.scalaz() ++
          depend.testing() ++
          Seq(
            "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
          )
      )
  )
}
