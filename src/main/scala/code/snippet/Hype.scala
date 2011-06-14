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

object Hype {
  val SearchEp1RE = """(.*?)[sS]([0-9]+)[eE]([0-9]+)(.*)""".r
  val SearchEp2RE = """(.*?)([0-9]+)[xX]([0-9]+)(.*)""".r
  val SearchSeasonRE = """(?i)(.*?)season ([0-9]+)(.*)""".r
  val SearchEpisodeRE = """(?i)(.*?)episode ([0-9]+)(.*)""".r
}

class Hype extends Logger {
  def updateShow (s: String) = if (s != "") {
    var req = s
    season = req match {
      case Hype.SearchSeasonRE(_, sh, _)    => Full(sh toLong)
      case Hype.SearchEp1RE   (_, sh, _, _) => Full(sh toLong)
      case Hype.SearchEp2RE   (_, sh, _, _) => Full(sh toLong)
      case _ => Empty
    }
    episode = req match {
      case Hype.SearchEpisodeRE(_, e, _)    => Full(e toLong)
      case Hype.SearchEp1RE    (_, _, e, _) => Full(e toLong)
      case Hype.SearchEp2RE    (_, _, e, _) => Full(e toLong)
      case _ => Empty
    }

    req = Hype.SearchEp1RE.replaceAllIn(req, m => m.group(1) + m.group(4))
    req = Hype.SearchEp2RE.replaceAllIn(req, m => m.group(4) + m.group(4))
    req = Hype.SearchSeasonRE.replaceAllIn(req, m => m.group(1) + m.group(3))
    req = Hype.SearchEpisodeRE.replaceAllIn(req, m => m.group(1) + m.group(3))

    debug ("Searching show: " + req + (season match { case Full(s) => " / Season " + s; case _ => "" }) + (episode match { case Full(e) => " / Episode " + e; case _ => "" }))

    shows = HypeShow.findAll (Like(HypeShow.title, "%" + (req replaceAll("""[. _\-]""", "%")) + "%"), OrderBy (HypeShow.title, Ascending))
  } else shows = List[HypeShow]()

  var search = ""
  var shows = List[HypeShow]()
  var season: Box[Long] = Empty
  var episode: Box[Long] = Empty

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
    ".episodes *" #> (".episode *" #> (show.files filter(ep => 
      (episode match {
        case Full(e) => e == ep.episode.is
        case _ => true
      }) && (season match {
        case Full(s) => s == ep.season.is
        case _ => true
      })
    ) map(renderEpisode)))
  }

  /**
   * Render the full list of shows
   */
  def renderList = ".series *" #> (shows filter(s => s.files.length != 0) map (renderShow))

  /**
   * Render the Reload button
   */
  def renderReload = "*" #> SHtml.idMemoize (outer =>
    "#reload [onclick]" #> SHtml.ajaxInvoke (() => { HypeFile.update(); updateShow(search); SetHtml (outer.latestId, renderList(outer.applyAgain())) })
  )

  def process(outer: IdMemoizeTransform): () => JsCmd = {
    () => {
      updateShow(search);
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
