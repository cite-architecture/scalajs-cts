enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "scalajscts"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases") //add resolver

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "com.lihaoyi" %%% "scalatags" % "0.6.3",
  "com.lihaoyi" %%% "scalarx" % "0.2.8",
	"edu.holycross.shot.cite" %%% "xcite" % "2.0.1",
	"edu.holycross.shot" %%% "ohco2" % "6.3.0"
)
