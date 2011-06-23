package trakt

import net.liftweb.json._
import net.liftweb.common.{Logger, Full, Empty, Box}
import net.liftweb.json.JsonParser

/**
 * Default object to add before parsing JSON from Trakt
 */
object Additions {
  implicit val defaults = DefaultFormats
  val API_KEY = "93f34432220b4d372501cff4f0caeb9c"
}

/**
 * Case class containing episode data
 */
case class Episode (season: Long,
                    number: Long,
                    title: String,
                    overview: String,
                    first_aired: Long,
                    images: EpisodeImages) {
  val date = new java.util.Date (first_aired*1000L)
  override def toString() = title + " (" + season + "x" + number + ")"
}

/**
 * Case class containing show images
 */
case class ShowImages (poster: String, fanart: String)

/**
 * Case class containing episode image
 */
case class EpisodeImages (screen: String)

/**
 * Case class containing show information
 */
case class Show (title: String,
                 overview: String,
                 air_day: String,
                 air_time: String,
                 tvdb_id: String,
                 images: ShowImages) {
  val id = tvdb_id.toLong
  override def toString() = title

  /**
   * Select one episode for this show
   */
  def episode (season: Long, episode: Long) = Episode.summary (id, season, episode)
}

/**
 * Static methods related to episodes
 */
object Episode extends Logger {
  import Additions._

  /**
   * Fetch episode data from Trakt
   */
  def summary (tvdb_id: Long, season: Long, episode: Long): Box[FullEpisode] = {
    info ("Fetching episode info for tvdb_id=" + tvdb_id);
    try {
      Full(parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/show/episode/summary.json/" + API_KEY + "/" + tvdb_id + "/" + season + "/" + episode).mkString).extract[FullEpisode])
    } catch { case e:java.io.IOException => { info(e); Empty } }
  }
}

/**
 * Full episode data, including show (used on some trakt JSON responses)
 */
case class FullEpisode (show: Show, episode: Episode)

/**
 * List of episodes by date
 */
case class DateList (date: String, episodes: List[FullEpisode])

/**
 * Contains personnal calendar
 */
case class Calendar (episodes: List[DateList])

/**
 * Contains basic Trakt fetching operations
 */
object Trakt extends Logger {
  import Additions._

  /**
   * Fetch show data
   */
  def show (name: String):Show = {
    val enc = java.net.URLEncoder.encode (name, "UTF-8");
    debug ("Sending request: " + "http://api-trakt.apigee.com/show/summary.json/" + API_KEY + "/" + enc)
    parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/show/summary.json/" + API_KEY + "/" + enc).mkString).extract[Show]
  }

  /**
   * Fetch user calendar
   */
  def calendar (user: String):Calendar = { 
    val enc = java.net.URLEncoder.encode (user, "UTF-8");
    debug ("Sending request: " + "http://api-trakt.apigee.com/user/calendar/shows.json/" + API_KEY + "/" + enc)
    Calendar (parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/user/calendar/shows.json/" + API_KEY + "/" + enc).mkString).extract[List[DateList]])
  }

  /**
   * Search for show
   */
  def searchShow (name: String): List[Show] = {
    val enc = java.net.URLEncoder.encode (name, "UTF-8");
    debug ("Sending request: " + "http://api-trakt.apigee.com/user/search/shows.json/" + API_KEY + "/" + enc)
    parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/search/shows.json/" + API_KEY + "/" + enc).mkString).extract[List[Show]]
  }

  /**
   * Select show based on TVDB ID
   */
  def selectShow (tvdb_id: Long): Show = {
    val enc = java.net.URLEncoder.encode (tvdb_id toString, "UTF-8");
    debug ("Sending request: " + "http://api-trakt.apigee.com/show/summary.json/" + API_KEY + "/" + enc)
    parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/show/summary.json/" + API_KEY + "/" + enc).mkString).extract[Show]
  }
}

/**
 * Some static methods for selecting user calendars
 */
object Calendar {
  def apply (u: String) = Trakt.calendar (u)
}

/**
 * Some static methods for selecting shows
 */
object Show {
  def apply (s: String) = Trakt.show (s)
  def search (name: String) = Trakt.searchShow (name)
  def select (tv: Long) = Trakt.selectShow(tv)
}
