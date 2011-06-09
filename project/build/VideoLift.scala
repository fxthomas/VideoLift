import sbt._

class VideoLift (info: ProjectInfo) extends DefaultWebProject(info) {
  val liftVersion = "2.3"

  // uncomment the following if you want to use the snapshot repo
  //  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  override def libraryDependencies = Set(
    "mysql-connector-java" % "mysql" % "5.1.6" from "http://mirrors.ibiblio.org/pub/mirrors/maven2/mysql/mysql-connector-java/5.1.6/mysql-connector-java-5.1.6.jar",
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "junit" % "junit" % "4.5" % "test",
    "ch.qos.logback" % "logback-classic" % "0.9.26",
    "org.scala-tools.testing" %% "specs" % "1.6.6" % "test"
  ) ++ super.libraryDependencies
}
