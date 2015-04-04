name := "budgetviz"

scalaVersion := "2.10.4"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "commons-io"                 %  "commons-io"      % "2.1",
  "io.spray" % "spray-can" % "1.3.1",
  "io.spray" % "spray-routing" % "1.3.1",
  "io.spray" %%  "spray-json" % "1.2.6"
//  "io.spray"                   %% "spray-json"      % "1.2.5"
//  "net.sf.supercsv"            %  "super-csv"       % "2.1.0"
)

Revolver.settings

