import AssemblyKeys._

name                := "Playground"

version             := "1.0"

scalaVersion        := "2.12.8"

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "org.scalaz" 		          %% "scalaz-core" 			  % "7.2.27",
  "org.scalaz.stream"       %% "scalaz-stream" 		  % "0.8.6",
  "com.chuusai"             %% "shapeless"          % "2.3.3",
  "org.specs2"              %% "specs2-core"        % "4.1.0" % "test"
)

scalacOptions       += "-feature"

initialCommands in console :=
  """import scalaz._, Scalaz._
    |import scalaz.stream._
    |import scalaz.concurrent.Task
  """.stripMargin

jarName   in assembly := "solution.jar"

mainClass in assembly := Some("com.playground.solution.Main")