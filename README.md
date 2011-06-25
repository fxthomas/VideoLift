VideoLift is a small automatic TV shows, movies,... library.

It fetches metadata automatically from file names :
  e.g. Awesome.Show.s01e02.hdtv.XVid.avi -> Awesome Show, Season 1, Episode 2, with lots of info on it

To run it, you must :

*   Use SBT (Simple Build Tool for Scala)
*   Edit the src/main/resources/props/default.props and add your DB settings and file library paths
*   For Jetty: Add the 'servlet-api.jar' file to a lib/ folder at the root, if you want to use 'jetty-run'
               (this file can usually be found at /usr/share/tomcat7/lib/ or something like that)
*   For Tomcat: Package (with 'package') and deploy the jar in target/.../videolift-...jar

Enjoy ;)
~ FX for the Hype* Team
