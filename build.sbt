name := "bayesian-personalized-ranking"

version := "0.1"

scalaVersion := "2.10.1"

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.3-SNAPSHOT), use this.
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
            "org.scalanlp" % "breeze-math_2.10" % "0.2.3",
            "org.scalanlp" % "breeze-learn_2.10" % "0.2.3",
            "org.scalanlp" % "breeze-process_2.10" % "0.2.3",
            "org.scalanlp" % "breeze-viz_2.10" % "0.2.3"
)

