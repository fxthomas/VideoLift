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
  val NewestRE = """(?i)(newest|latest|new)(.*)""".r
}

class Hype extends Logger {
  /**
   * Current search pattern
   */
  var search = ""

  /**
   * Current list of shows
   */
  var shows = List[HypeShow]()

  /**
   * Current requested season
   */
  var season: Box[Long] = Empty

  /**
   * Current requested episode
   */
  var episode: Box[Long] = Empty

  /**
   * Do we want the latest episode?
   */
  var newest = false;

  /**
   * Update variables based on request, and fetch info from DB
   */
  def updateShow (s: String) = {
    var req = s

    /*
     * Do we want the newest episode only?
     */
    newest = req match {
      case Hype.NewestRE(_, _) => true
      case _ => false
    }

    /*
     * Do we want a specific season?
     */
    season = req match {
      case Hype.SearchSeasonRE(_, sh, _)    => Full(sh toLong)
      case Hype.SearchEp1RE   (_, sh, _, _) => Full(sh toLong)
      case Hype.SearchEp2RE   (_, sh, _, _) => Full(sh toLong)
      case _ => Empty
    }

    /*
     * Do we want a specific episode?
     */
    episode = req match {
      case Hype.SearchEpisodeRE(_, e, _)    => Full(e toLong)
      case Hype.SearchEp1RE    (_, _, e, _) => Full(e toLong)
      case Hype.SearchEp2RE    (_, _, e, _) => Full(e toLong)
      case _ => Empty
    }

    /*
     * Remove all special requests
     */
    req = Hype.SearchEp1RE.replaceAllIn(req, m => m.group(1) + m.group(4))
    req = Hype.SearchEp2RE.replaceAllIn(req, m => m.group(4) + m.group(4))
    req = Hype.SearchSeasonRE.replaceAllIn(req, m => m.group(1) + m.group(3))
    req = Hype.SearchEpisodeRE.replaceAllIn(req, m => m.group(1) + m.group(3))
    req = Hype.NewestRE.replaceAllIn (req, m => m.group(2))
    req = req.replaceAll ("""[^a-zA-Z0-9]""", " ")

    /*
     * Fetch info from DB
     */
    if (req != "") {
      debug ("Searching show: " + req + (season match { case Full(s) => " / Season " + s; case _ => "" }) + (episode match { case Full(e) => " / Episode " + e; case _ => "" }))
      shows = HypeShow.findAll (Like(HypeShow.title, "%" + (req replaceAll(""" """, "%")) + "%"), OrderBy (HypeShow.title, Ascending))
    }
    /*
     * If the request if empty, don't display anything
     */
    else shows = List[HypeShow]()
  }

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
    /*
     * Render series info
     */
    ".series-info" #> (
      ".picture *" #> (
        (if (show.image != "") "img [src]" #> show.image else "img" #> "") &
        ".showtitle *" #> show.title &
        (if (show.tvdb_id != 0) (".showtitle [href]" #> ("http://thetvdb.com/?tab=series&id="+show.tvdb_id)) else ".showtitle [href]" #> "")
      ) & ".description *" #> show.description
    ) &
    /*
     * Render episode list
     */
    ".episodes *" #> (".episode *" #> ((

      /*
       * If we're not looking for the newest episode, look for episodes matching ep/season request, if need be
       */
      if (!newest) {
        show.files filter(ep => 
          (episode match {
            case Full(e) => e == ep.episode.is
            case _ => true
          }) && (season match {
            case Full(s) => s == ep.season.is
            case _ => true
          })
        )

      /*
       * If we're looking for the newest epiosde, find it
       */
      } else {
        var hsm = show.files(0)
        show.files foreach (f => hsm = if (f.season.is > hsm.season.is || (f.season.is == hsm.season.is && f.episode.is > hsm.episode.is)) f else hsm)
        List(hsm)
      }

    /*
     * Render each episode
     */
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

  /**
   * Process the AJAX search request
   */
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
