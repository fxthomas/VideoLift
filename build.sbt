//
// Project properties
// Fri Jun 03 16:04:40 CEST 2011
//

organization := "Telecom ParisTech"

name := "VideoLift"

version := "1.0"

scalaVersion := "2.9.1"

seq (webSettings :_*)

libraryDependencies ++= {
    val liftVersion = "2.4-M4"
    Seq(
      "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
      "net.liftweb" %% "lift-mapper" % liftVersion % "compile",
      "org.mortbay.jetty" % "jetty" % "6.1.22" % "container",
      "ch.qos.logback" % "logback-classic" % "0.9.26"
    )
}

libraryDependencies += "mysql-connector-java" % "mysql" % "5.1.6" from "http://mirrors.ibiblio.org/pub/mirrors/maven2/mysql/mysql-connector-java/5.1.6/mysql-connector-java-5.1.6.jar"

libraryDependencies += "junit" % "junit" % "4.5" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.26"

libraryDependencies += "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"

libraryDependencies += "javax.servlet" % "servlet-api" % "2.5"
