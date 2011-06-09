package trakt

import net.liftweb.json._
import net.liftweb.json.JsonParser

object Additions {
  implicit def toPIMPString (s: String) = new {
    def fwi (padding: Int, max_width: Int) = {
      val cmax  = max_width - padding;
      var count = 0;
      s.foldLeft ("") {(total, cn) =>
        count += 1;
        total + cn +
        {cn match {
          case ' '|'.'|'!'|'?'|';'|','|'\n' => if (count >= cmax) {count = 0; "\n" + (" "*padding)} else ""
          case _ => ""
        }}
      }.replaceAll ("(?m)^ {"+padding+"} *", " "*padding)
    }
  }
}

case class Episode (season: Int,
                    number: Int,
                    title: String,
                    overview: String,
                    first_aired: Long) {
  import Additions._
  val date = new java.util.Date (first_aired*1000L)

  def printString() = {
    "Episode Title : " + title + " (" + season + "x" + number + ")" + "\n" + 
    "Ep. Overview  : " + overview.fwi(16, 80) + "\n" +
    "Air Date      : " + date + "\n"
  }

  def print() = println (printString())

  override def toString() = title + " (" + season + "x" + number + ")"
}

case class Show (title: String,
                 overview: String,
                 air_day: String,
                 air_time: String) {
  import Additions._

  def printString() = {
    {"Show Name     : " + title + "\n"} +
    {"Overview      : " + overview.fwi(16, 80) + "\n"} +
    {"Scheduled     : " + air_day + " at " + air_time + "\n"}
  }

  def print() = println (printString())

  override def toString() = title
}

case class FullEpisode (show: Show, episode: Episode) {
  def printString() = show.printString + episode.printString + {"-"*80} + "\n"
  def print() = println (printString())
}

case class DateList (date: String, episodes: List[FullEpisode]) {
  def printString() = " [ Date: " + date + "]\n" + {"-" * 80} + "\n" + {episodes.foldLeft("") {(total: String, fep: FullEpisode) => total + fep.printString()}} + "\n"
  def print() = println (printString())
}

case class Calendar (episodes: List[DateList]) {
  def printString() = episodes.foldLeft("") {(total: String, fep: DateList) => total + fep.printString()} + "\n"
  def print() = println (printString())
}

object Trakt {
  implicit val defaults = DefaultFormats
  val API_KEY = "93f34432220b4d372501cff4f0caeb9c"

  def show (name: String):Show = {
    val enc = java.net.URLEncoder.encode (name, "UTF-8");
    parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/show/summary.json/" + API_KEY + "/" + enc).mkString).extract[Show]
  }

  def calendar (user: String):Calendar = { 
    val enc = java.net.URLEncoder.encode (user, "UTF-8");
    Calendar (parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/user/calendar/shows.json/" + API_KEY + "/" + enc).mkString).extract[List[DateList]])
  }

  def searchShow (name: String): List[Show] = {
    val enc = java.net.URLEncoder.encode (name, "UTF-8");
    parse (scala.io.Source.fromURL ("http://api-trakt.apigee.com/search/shows.json/" + API_KEY + "/" + enc).mkString).extract[List[Show]]
  }
}

object Calendar {
  def apply (u: String) = Trakt.calendar (u)
}

object Show {
  def apply (s: String) = Trakt.show (s)
  def search (name: String) = Trakt.searchShow (name)
}
