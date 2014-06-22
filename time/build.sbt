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

uniform.project("omnitool-time", "au.com.cba.omnia.omnitool.time")

uniformDependencySettings

libraryDependencies :=
  depend.time() ++
  depend.scalaz() ++
  depend.testing() ++ Seq(
    "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
  )

autoAPIMappings := true

apiMappings ++= {
  val cp: Seq[Attributed[File]] = (fullClasspath in Compile).value
  def findManagedDependency(organization: String, name: String): File = {
    ( for {
      entry <- cp
      module <- entry.get(moduleID.key)
      if module.organization == organization
      if module.name.startsWith(name)
      jarFile = entry.data
    } yield jarFile
    ).head
  }
  Map(
    findManagedDependency("joda-time", "joda-time") -> url("http://www.joda.org/joda-time/apidocs/"),
    findManagedDependency(
      "org.scalaz", "scalaz-core") ->
      url(s"http://scalaz.github.io/scalaz/scalaz-2.10-${depend.versions.scalaz}/doc/"
    )
  )
}
