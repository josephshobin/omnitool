uniform.project("omnitool-parser", "au.com.cba.omnia.omnitool.parser")

uniformDependencySettings

libraryDependencies :=
  depend.scalaz() ++
  Seq(
    "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
  )
