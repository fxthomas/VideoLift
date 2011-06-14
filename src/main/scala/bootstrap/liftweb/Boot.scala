package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.mapper._

import Helpers._
import javax.servlet.http.HttpServletRequest

import net.liftweb.mapper.{ConnectionIdentifier, ConnectionManager, Schemifier}
import java.sql.{Connection, DriverManager}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.Props

import code.model._

/**
* A class that's instantiated early and run.  It allows the application
* to modify lift's environment
*/
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager (DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User)
    Schemifier.schemify(true, Schemifier.infoF _, HypeMapper)
    Schemifier.schemify(true, Schemifier.infoF _, HypeShow)
    Schemifier.schemify(true, Schemifier.infoF _, HypeFile)
    
    HypeFile.update()

    // where to search snippet
    LiftRules.addToPackages("code")
    LiftRules.early.append(makeUtf8) 

    // Define SiteMap
    def sitemap(): SiteMap = SiteMap (
      Menu.i ("VideoLift") / "index",
      Menu.i ("VideoLift | Help") / "help",
      Menu.i ("Media") / "media" / **
    )

    // Tell Lift where to find the sitemap
    LiftRules.setSiteMapFunc(() => sitemap())

    // Add AJAX spinner gifs
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show ("ajax-loader").cmd)
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide ("ajax-loader").cmd)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent)
    )

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
  private def makeUtf8(req: HTTPRequest){ 
    req.setCharacterEncoding("UTF-8") 
  }
}
