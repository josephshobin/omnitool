uniform.project("omnitool-core", "au.com.cba.omnia.omnitool.core")

uniformDependencySettings

libraryDependencies :=
  depend.scalaz() ++ depend.testing() ++
  Seq(
    "au.com.cba.omnia" %% "omnia-test" % "2.0.0-20140507011446-f9e9d08" % "test"
  )
