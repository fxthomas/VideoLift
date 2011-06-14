package code
package snippet

import scala.xml.{NodeSeq, Text}

import net.liftweb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props 
import net.liftweb.http.{ SHtml, IdMemoizeTransform }
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.{OrderBy, Ascending}

import model.{HypeFile, HypeShow}

object Hype {
  def findShows = HypeShow.findAll(OrderBy (HypeShow.title, Ascending))
  var shows = findShows
}

class Hype {
  /**
   * Render one episode
   */
  def renderEpisode (ep: HypeFile) = {
    ".episode-picture *" #> (
      "img [src]" #> ep.image &
      ".download [href]" #> ((Props.get("library.url") openOr "/home/fx/Videos/") + ep.filename)
    ) &
    (if (ep.season != 0)  ".season *"  #> ("Season " + ep.season)   else ".season" #> "") &
    (if (ep.episode != 0) ".episode *" #> ("Episode " + ep.episode) else ".episode" #> "") &
    (if (ep.title != 0)   ".title *"   #> (ep.title)                else ".title" #> "") &
    ".description *" #> ep.description
  }

  /**
   * Render one show
   */
  def renderShow (show: HypeShow) = {
    ".series-info" #> (
      ".picture *" #> (
        "img [src]" #> show.image &
        ".showtitle *" #> show.title
      ) & ".description *" #> show.description
    ) &
    ".episodes *" #> (".episode *" #> show.files.map (renderEpisode))
  }

  /**
   * Render the full list of shows
   */
  def renderList = ".series *" #> (Hype.shows filter(s => s.files.length != 0) map (renderShow))

  /**
   * Finish template bindings
   */
  def render = "*" #> renderList &
    "*" #> SHtml.idMemoize (outer =>
      "#reload [onclick]" #> SHtml.ajaxInvoke (() => { HypeFile.update(); Hype.shows = Hype.findShows; SetHtml (outer.latestId, renderList(outer.applyAgain())) })
    )
}
