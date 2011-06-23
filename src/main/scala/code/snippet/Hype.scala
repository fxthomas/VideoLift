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
import net.liftweb.http.js.jquery.JqJsCmds.{Show, Hide}
import net.liftweb.mapper.{By, Like, OrderBy, Ascending}

import java.util.Random

import model.{HypeFile, HypeEpisode, HypeShow, HypeMovie}

object Hype {
  val SearchEp1RE = """(.*?)[sS]([0-9]+)[eE]([0-9]+)(.*)""".r
  val SearchEp2RE = """(.*?)([0-9]+)[xX]([0-9]+)(.*)""".r
  val SearchSeasonRE = """(?i)(.*?)season ([0-9]+)(.*)""".r
  val SearchEpisodeRE = """(?i)(.*?)episode ([0-9]+)(.*)""".r
  val NewestRE = """(?i)(newest|latest|new)(.*)""".r
  val RandomRE = """(?i)random""".r

  val RandSeed = new Random(System.currentTimeMillis());
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
   * Current movie list
   */
  var movies = List[HypeMovie]()

  /**
   * Current misc. list
   */
  var miscs = List[HypeFile]()

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
   * Do we want a random show?
   */
  var random = false;

  /**
   * Do we display the section?
   */
  var display_shows = false;
  var display_movies = false;
  var display_misc = false;

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
     * Do we want a random series?
     */
    random = req match {
      case Hype.RandomRE() => true
      case _ => false
    }

    if (!random) {
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

      /*
       * Optimise string for search
       */
      req = req.replaceAll ("""[^a-zA-Z0-9]""", " ")
      req = req.replaceAll ("""^ +""", "")
      req = req.replaceAll (""" +$""", "")
    } else req = ""

    /*
     * Fetch info from DB
     */
    if (req != "" || random) {
      debug ("Searching show: " + req + (season match { case Full(s) => " / Season " + s; case _ => "" }) + (episode match { case Full(e) => " / Episode " + e; case _ => "" }))
      shows = HypeShow.findAll (Like(HypeShow.title, "%" + (req replaceAll(""" """, "%")) + "%"), OrderBy (HypeShow.title, Ascending))

      debug ("Searching movie: " + req)
      movies = HypeMovie.findAll (Like(HypeMovie.title, "%" + (req replaceAll(""" """, "%")) + "%"), OrderBy (HypeMovie.title, Ascending))

      debug ("Searching miscs: " + req)
      miscs = HypeFile.findAll (Like (HypeFile.filename, "%" + (req replaceAll(""" """, "%")) + "%"), By (HypeFile.episode, Empty), By (HypeFile.movie, Empty), OrderBy(HypeFile.filename, Ascending))
    }
    /*
     * If the request if empty, don't display anything
     */
    else {
      shows = List[HypeShow]()
      movies = List[HypeMovie]()
      miscs = List[HypeFile]()
    }
  }

  /**
   * Render one episode
   */
  def renderEpisode (ep: HypeEpisode) = {
    ".episode-picture *" #> (
      (if (ep.image != "") "img [src]" #> ep.image else "img" #> "") &
      ".download *" #> (".links *" #> ("._link *" #> (ep.files map(f => {
        "a [href]" #> ((Props.get("library.url") openOr "/home/fx/Videos/") + f.filename) &
        "a *" #> ("span *" #> (f info))
      }))))
    ) &
    (if (ep.season != 0)  ".season *"  #> ("Season " + ep.season)   else ".season" #> "") &
    (if (ep.episode != 0) ".episode *" #> ("Episode " + ep.episode) else ".episode" #> "") &
    (if (ep.title != "")  ".title *"   #> (ep.title)                else ".title" #> "") &
    ".description *" #> ep.description
  }

  def renderMovie (movie: HypeMovie) = {
    ".picture *" #> (
      (if (movie.image != "") "img [src]" #> movie.image else "img" #> "") &
      ".download *" #> (".links *" #> ("._link *" #> (movie.files map(f => {
        "a [href]" #> ((Props.get("library.url") openOr "/home/fx/Videos/") + f.filename) &
        "a *" #> ("span *" #> (f info))
      }))))
    ) &
    (if (movie.title != "") ".title *" #> (movie.title) else ".title" #> "") &
    (if (movie.year != "") ".year *" #> (movie.year) else ".year" #> "") &
    (if (movie.duration != 0) ".duration *" #> (movie.duration) else ".duration" #> "") &
    ".description *" #> movie.description
  }

  def renderMisc (misc: HypeFile) = {
    ".picture *" #> (
      "img" #> "" &
      ".download *" #> (".links *" #> ("._link *" #> {
        "a [href]" #> ((Props.get("library.url") openOr "/home/fx/Videos/") + misc.filename) &
        "a *" #> ("span *" #> (misc info))
      }))
    ) &
    ".title *" #> (misc.filename)
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
  def renderList = ".series *" #> {
    if (!random) {
      val sss = (shows filter(s => s.files.length != 0))
      if (sss isEmpty) display_shows = false
      else display_shows = true
      sss map (renderShow)
    } else {
      val sf = shows filter (s => s.files nonEmpty);
      if (sf nonEmpty) {
        val rand = Hype.RandSeed.nextInt (sf.length);
        display_shows = true
        List(renderShow (sf(rand)))
      } else {
        display_shows = false
        List()
      }
    }
  } & ".movies *" #> {
    if (!random) {
      val mmm = (movies filter (s => s.files.length != 0))
      if (mmm isEmpty) display_movies = false
      else display_movies = true
      mmm map (renderMovie)
    } else {
      val sf = movies filter (s => s.files nonEmpty);
      if (sf nonEmpty) {
        val rand = Hype.RandSeed.nextInt (sf.length);
        display_movies = true
        List (renderMovie (sf(rand)))
      } else {
        display_movies = false
        List()
      }
    }
  } & ".miscs *" #> {
    if (miscs isEmpty) display_misc = false
    else display_misc = true

    if (!random) {
      miscs map (renderMisc)
    } else {
      if (miscs nonEmpty) {
        val rand = Hype.RandSeed.nextInt (miscs.length);
        List (renderMisc(miscs(rand)))
      } else List()
    }
  }

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
      SetHtml (outer.latestId, render(outer.applyAgain())) & (
        if (display_misc) Show("misc-header")
        else Hide ("misc-header")
      ) & (
        if (display_movies) Show("movies-header")
        else Hide ("movies-header")
      ) & (
        if (display_shows) Show("series-header")
        else Hide("series-header")
      )
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
