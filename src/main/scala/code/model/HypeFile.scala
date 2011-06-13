package code
package model

import net.liftweb._
import util.Props
import mapper._
import common._
import scala.tools.nsc.io.Path
import scala.util.matching.Regex
import trakt._

class HypeFile extends LongKeyedMapper[HypeFile] {
  def getSingleton = HypeFile
  def primaryKeyField = id

  object id extends MappedLongIndex (this)
  object filename extends MappedText (this)
  object imdb_id extends MappedLong (this)
  object title extends MappedString (this, 256)
  object show extends LongMappedMapper (this, HypeShow)
  object season extends MappedLong (this)
  object episode extends MappedLong (this)
  object description extends MappedText (this)
  object image extends MappedString (this, 256)
}

object HypeMisc extends Logger {
  def create (name: String) = {
    info ("Misc: " ++ name)
    (HypeFile create) title(name) filename(name)
  }
}

object HypeFile extends HypeFile with LongKeyedMetaMapper[HypeFile] with Logger {
  override def dbTableName = "files"

  val ExtRE = """.*\.(mkv|MKV|avi|AVI|mp4|MP4|ogm|OGM)$""".r
  val AnimeRE = new Regex ("""(.+?/)*\[([a-zA-Z0-9_]+?)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)-( |_)([0-9]+)(.*)""", "dir", "source", "", "name", "", "", "episode", "info")
  val Anime2RE = new Regex ("""(.+?/)*\[([a-zA-Z0-9_]+?)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)([0-9]+)(.*)""", "dir", "source", "", "name", "", "episode", "info")
  val Anime3RE = new Regex ("""(.+?/)*_*([a-zA-Z0-9_]+?)_+([a-zA-Z0-9_ \-?!]+)_-_([0-9]+)_+(.*)""", "dir", "source", "name", "episode", "info")
  val Anime4RE = new Regex ("""(.+?/)*_*([a-zA-Z0-9_]+?)_+([a-zA-Z0-9_ \-?!]+)_+([0-9]+)_+(.*)""", "ir", "source", "name", "episode", "info")
  val TVRE = new Regex ("""(.+?/)*([a-zA-Z0-9_.]+)\.[sS]([0-9]+)[eE]([0-9]+)(.*)""", "dir", "name", "season", "episode", "info")
  val TV2RE = new Regex ("""(.+?/)*([a-zA-Z0-9_.]+)\.([0-9]+)[xX]([0-9]+)(.*)""", "dir", "name", "season", "episode", "info")
  val MovieRE = new Regex ("""(.+?/)*([a-zA-Z0-9.]+)\.([0-9]{4})\.(720p|1080p|HDTV)\.([a-zA-Z0-9.]+)-([a-zA-Z0-9.]+)""", "dir", "name", "year", "format", "codec", "source")

  implicit def toClean (s: String) = new { def clean = s replaceAll ("^[\\-_ ]+", "") replaceAll ("[\\-_ ]+$", "") replaceAll ("[_.]", " ") }

  def createEpisode (_name: String, filename: String, season: Int, episode: Int): HypeFile = {
    info ("Searching file: " + filename + " (Season = " + season + ", Episode = " + episode + ")")

    // Find show filename mapping if there is one
    val mapping = HypeMapper.findAll (By (HypeMapper.original, _name))
    val show = if (mapping.length > 0) {
      val hs = HypeShow.findAll (By (HypeShow.tvdb_id, mapping(0).map.is))
      if (hs.length > 0) hs(0) else { HypeShow.createShow(_name, mapping(0).map.is) }
    } else {
      val hs = HypeShow.findAll (By (HypeShow.title, _name))
      if (hs.length > 0) hs(0) else { HypeShow.createShow(_name) }
    }

    // Create Episode
    val hf = (HypeFile.create) show(show) filename(filename) season(season) episode(episode);

    // Search for episode
    Episode.summary (show.tvdb_id, season, episode) match {
      case Full(eps) => { hf description(eps.episode.overview); hf image(eps.episode.images.screen); hf title(eps.episode.title) }
      case _ => hf description("No description")
    }
  }

  def update () {
    val dir = Props.get ("library.path") openOr "/home/fx/Videos/"
    info ("Upating directory structure: " ++ dir)

    val fslist = Path (new java.io.File (dir)).walk map (_.path replaceAll("""^""" + dir, "")) filter(_ match { case ExtRE(ext) => true; case _ => false }) toList
    val fulldblist = HypeFile.findAll ()
    val dblist = fulldblist filter(d => fslist contains d.filename.is) map (_.filename)

    // Remove all shows that aren't in the file system anymore
    fulldblist filter(d => !(fslist contains d.filename.is)) foreach (_ delete_!)

    // Parse new files for shows
    fslist
      .filter (f => !(dblist contains f))
      .map (_ match {
        case n@AnimeRE (dir, source, _, name, _, _, episode, inf) => HypeFile.createEpisode (name clean, n, 1, episode toInt)
        case n@Anime2RE (dir, source, _, name, _, episode, inf) => HypeFile.createEpisode (name clean, n, 1, episode toInt)
        case n@Anime3RE (dir, source, name, episode, inf) => HypeFile.createEpisode (name clean, n, 1, episode toInt)
        case n@Anime4RE (dir, source, name, episode, inf) => HypeFile.createEpisode (name clean, n, 1, episode toInt)
        case n@MovieRE (dir, name, year, format, codec, source) => (HypeFile.create) title(name clean) filename(n)
        case n@TVRE (dir, name, season, episode, inf) => HypeFile.createEpisode (name clean, n, season toInt, episode toInt)
        case n@TV2RE (dir, name, season, episode, inf) => HypeFile.createEpisode (name clean, n, season toInt, episode toInt)
        case n@_ => (HypeFile.create) title(n) filename(n)
      }
    ).foreach (_ save)
  }
}
