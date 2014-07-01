scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0-M7"
)

scalacOptions ++= Seq("-deprecation", "-language:_")

DoctestPlugin.doctestSettings

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
