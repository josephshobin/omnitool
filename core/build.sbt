uniform.project("omnitool-core", "au.com.cba.omnia.omnitool.core")

uniformDependencySettings

libraryDependencies :=
  depend.scalaz() ++ depend.testing() ++
  Seq(
    "au.com.cba.omnia" %% "omnia-test" % "2.1.0-20140604032817-d3b19f6" % "test"
  )
