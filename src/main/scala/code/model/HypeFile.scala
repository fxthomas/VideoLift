package code
package model

import net.liftweb._
import util.Props
import mapper._
import common._
import scala.tools.nsc.io.Directory
import scala.util.matching.Regex
import trakt._

class HypeFile extends LongKeyedMapper[HypeFile] {
  def getSingleton = HypeFile
  def primaryKeyField = id

  object id extends MappedLongIndex (this)
  object filetype extends MappedEnum[HypeFile, HypeFile.Kind.type](this, HypeFile.Kind)
  object filename extends MappedText (this)
  object tvdb_id extends MappedLong (this)
  object imdb_id extends MappedLong (this)
  object title extends MappedString (this, 256)
  object series extends MappedString (this, 256)
  object season extends MappedLong (this)
  object episode extends MappedLong (this)
  object description extends MappedText (this)
  object image extends MappedString (this, 256)
}

object HypeShow extends Logger {
  def create (name: String, filename: String) = {
    val hf = HypeFile create;
    hf filetype(HypeFile.Kind.TV)
    hf filename(filename)

    info ("Searching Show: " ++ filename)
    val shows = Show.search(name)
    if (shows.length > 0) {
      info ("Show found: " ++ shows(0).title)
      hf title (shows(0).title)
      hf description (shows(0).overview)
    } else {
      hf title(name)
    }
  }
}

object HypeMisc extends Logger {
  def create (name: String) = {
    info ("Misc: " ++ name)
    (HypeFile create) title(name) filetype(HypeFile.Kind.Misc) filename(name)
  }
}

object HypeFile extends HypeFile with LongKeyedMetaMapper[HypeFile] with Logger {
  override def dbTableName = "files"

  object Kind extends Enumeration {
    val Misc = Value (0, "Miscellaneous file")
    val TV = Value (1, "TV Episode")
    val Movie = Value (2, "Movie")
  }

  val AnimeRE = new Regex ("""\[([a-zA-Z0-9_]+)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)-( |_)([0-9]+)(.*)""", "source", "", "name", "", "", "episode", "info")
  val Anime2RE = new Regex ("""\[([a-zA-Z0-9_]+)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)([0-9]+)(.*)""", "source", "", "name", "", "episode", "info")
  val Anime3RE = new Regex ("""_*([a-zA-Z0-9_]+)_+([a-zA-Z0-9_ \-?!]+)_-?_*([0-9]+)_+(.*)""", "source", "name", "episode", "info")
  val TVRE = new Regex ("""([a-zA-Z0-9_.]+)\.[sS]([0-9]+)[eE]([0-9]+)(.*)""", "name", "season", "episode", "info")
  val TV2RE = new Regex ("""([a-zA-Z0-9_.]+)\.([0-9]+)[xX]([0-9]+)(.*)""", "name", "season", "episode", "info")
  val MovieRE = new Regex ("""([a-zA-Z0-9.]+)\.([0-9]{4})\.(720p|1080p|HDTV)\.([a-zA-Z0-9.]+)-([a-zA-Z0-9.]+)""", "name", "year", "format", "codec", "source")

  implicit def toClean (s: String) = new { def clean = s replaceAll ("^[\\-_]+", "") replaceAll ("[\\-_]+$", "") replaceAll ("[_.]", " ") }

  def updateFromDirectory (dir: String) {
    info ("Upating directory structure: " ++ dir)

    val dlist = Directory(dir).files
    dlist map (_.name match {
        case n@AnimeRE (source, _, name, _, _, episode, inf) => HypeShow.create (name clean, n)
        case n@Anime2RE (source, _, name, _, episode, inf) => HypeShow.create (name clean, n)
        case n@Anime3RE (source, name, episode, inf) => HypeShow.create (name clean, n)
        case n@MovieRE (name, year, format, codec, source) => HypeShow.create (name clean, n)
        case n@TVRE (name, season, episode, inf) => HypeShow.create (name clean, n)
        case n@TV2RE (name, season, episode, inf) => HypeShow.create (name clean, n)
        case n@_ => HypeMisc.create (n)
      }
    ) foreach (_ save)
  }
}
