uniform.project("omnitool-parser", "au.com.cba.omnia.omnitool.parser")

uniformDependencySettings

libraryDependencies :=
  depend.scalaz() ++
  Seq(
    "au.com.cba.omnia" %% "omnia-test" % "2.0.0-20140507011446-f9e9d08" % "test"
  )
