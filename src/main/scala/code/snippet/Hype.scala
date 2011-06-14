package code
package snippet

import scala.xml.{NodeSeq, Text}

import net.liftweb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props 
import net.liftweb.http.{ S, SHtml, IdMemoizeTransform }
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.mapper.{Like, OrderBy, Ascending}

import model.{HypeFile, HypeShow}

class Hype extends Logger {
  def searchShow (s: String) = if (s != "") {
    debug ("Searching show: " + s)
    HypeShow.findAll (Like(HypeShow.title, "%" + (s replaceAll("""[. _\-]""", "%")) + "%"), OrderBy (HypeShow.title, Ascending))
  } else List[HypeShow]()

  var search = ""
  var shows = List[HypeShow]()

  /**
   * Render one episode
   */
  def renderEpisode (ep: HypeFile) = {
    ".episode-picture *" #> (
      (if (ep.image != "") "img [src]" #> ep.image else "img" #> "") &
      ".download [href]" #> ((Props.get("library.url") openOr "/home/fx/Videos/") + ep.filename)
    ) &
    (if (ep.season != 0)  ".season *"  #> ("Season " + ep.season)   else ".season" #> "") &
    (if (ep.episode != 0) ".episode *" #> ("Episode " + ep.episode) else ".episode" #> "") &
    (if (ep.title != "")  ".title *"   #> (ep.title)                else ".title" #> "") &
    ".description *" #> ep.description
  }

  /**
   * Render one show
   */
  def renderShow (show: HypeShow) = {
    ".series-info" #> (
      ".picture *" #> (
        (if (show.image != "") "img [src]" #> show.image else "img" #> "") &
        ".showtitle *" #> show.title &
        (if (show.tvdb_id != 0) (".showtitle [href]" #> ("http://thetvdb.com/?tab=series&id="+show.tvdb_id)) else ".showtitle [href]" #> "")
      ) & ".description *" #> show.description
    ) &
    ".episodes *" #> (".episode *" #> show.files.map (renderEpisode))
  }

  /**
   * Render the full list of shows
   */
  def renderList = ".series *" #> (shows filter(s => s.files.length != 0) map (renderShow))

  /**
   * Render the Reload button
   */
  def renderReload = "*" #> SHtml.idMemoize (outer =>
    "#reload [onclick]" #> SHtml.ajaxInvoke (() => { HypeFile.update(); shows = searchShow(search); SetHtml (outer.latestId, renderList(outer.applyAgain())) })
  )

  def process(outer: IdMemoizeTransform): () => JsCmd = {
    () => {
      shows = searchShow (search);
      SetHtml (outer.latestId, render(outer.applyAgain()))
    }
  }

  /**
   * Render search field
   */
  def renderSearch = "*" #> SHtml.idMemoize (outer =>
    "#search-form" #> (
      "name=show" #> (SHtml.text(search, search = _) ++ SHtml.hidden ( process (outer) ))
    )
  )

  /**
   * Finish template bindings
   */
  def render = "*" #> renderList & renderSearch & renderReload
}
