package code
package model

import net.liftweb._
import net.liftweb.util.Props
import net.liftweb.mapper._
import net.liftweb.common._

import scala.util.matching.Regex
import scala.tools.nsc.io.Path

import trakt._

class HypeFile extends LongKeyedMapper[HypeFile] {
  def getSingleton = HypeFile
  def primaryKeyField = id
  
  object id extends MappedLongIndex (this)
  object title extends MappedString (this, 256)
  object year extends MappedLong (this)
  object filename extends MappedString (this, 256)
  object codec extends MappedEnum[HypeFile, HypeFile.CodecType.type] (this, HypeFile.CodecType)
  object quality extends MappedEnum[HypeFile, HypeFile.QualityType.type] (this, HypeFile.QualityType)
  object episode extends MappedLongForeignKey (this, HypeEpisode)
  object movie extends MappedLongForeignKey (this, HypeMovie)
  object full_info extends MappedText (this)

  def info = {
    (if (quality.is != HypeFile.QualityType.Other) quality.is.toString else "") +
    (if (codec.is != HypeFile.CodecType.Other && quality.is != HypeFile.QualityType.Other) " | " else "") +
    (if (codec.is != HypeFile.CodecType.Other) codec.is.toString else "")
  }
}

object HypeFile extends HypeFile with LongKeyedMetaMapper[HypeFile] with Logger {
  override def dbTableName = "files"

  object CodecType extends Enumeration {
    val H264 = Value ("H264")
    val XviD = Value ("XviD")
    val Other = Value ("Other")
  }

  object QualityType extends Enumeration {
    val SD = Value ("SD")
    val HD = Value ("HD")
    val FullHD = Value ("Full HD")
    val Other = Value ("Other")
  }

  val ExtRE = """.*\.(mkv|MKV|avi|AVI|mp4|MP4|ogm|OGM)$""".r
  val AnimeRE = new Regex ("""(.+?/)*\[([a-zA-Z0-9_]+?)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)-( |_)([0-9]+)(.*)""", "dir", "source", "", "name", "", "", "episode", "info")
  val Anime2RE = new Regex ("""(.+?/)*\[([a-zA-Z0-9_]+?)\]( |_)([a-zA-Z0-9_ \-?!]+)( |_)([0-9]+)(.*)""", "dir", "source", "", "name", "", "episode", "info")
  val Anime3RE = new Regex ("""(.+?/)*_*([a-zA-Z0-9_]+?)_+([a-zA-Z0-9_ \-?!]+)_-_([0-9]+)_+(.*)""", "dir", "source", "name", "episode", "info")
  val Anime4RE = new Regex ("""(.+?/)*_*([a-zA-Z0-9_]+?)_+([a-zA-Z0-9_ \-?!]+)_+([0-9]+)_+(.*)""", "ir", "source", "name", "episode", "info")
  val TVRE = new Regex ("""(.+?/)*([a-zA-Z0-9_.]+)\.[sS]([0-9]+)[eE]([0-9]+)(.*)""", "dir", "name", "season", "episode", "info")
  val TV2RE = new Regex ("""(.+?/)*([a-zA-Z0-9_.]+)\.([0-9]+)[xX]([0-9]+)(.*)""", "dir", "name", "season", "episode", "info")
  val MovieRE = new Regex ("""(.+?/)*([a-zA-Z0-9.\-]+)\.([0-9]{4})\.(.*)""", "dir", "name", "year", "info")

  val CodecH264RE = new Regex ("""(?i).*(x264|h264).*""")
  val CodecXviDRE = new Regex ("""(?i).*(xvid).*""")
  val QualitySDRE = new Regex ("""(?i).*(sd|dvdrip).*""")
  val QualityHDRE = new Regex ("""(?i).*(720p).*""")
  val QualityFullHDRE = new Regex ("""(?i).*(1080p).*""")

  implicit def toClean (s: String) = new { def clean = s replaceAll ("^[\\-_ ]+", "") replaceAll ("[\\-_ ]+$", "") replaceAll ("[_.]", " ") }

  def createMisc (fn: String) = {
    val hf = (HypeFile create) filename(fn) title(fn)

    val codec = fn match {
      case CodecH264RE(_) => CodecType.H264
      case CodecXviDRE(_) => CodecType.XviD
      case _ => CodecType.Other
    }
    
    val quality = fn match {
      case QualitySDRE(_) => QualityType.SD
      case QualityHDRE(_) => QualityType.HD
      case QualityFullHDRE(_) => QualityType.FullHD
      case _ => QualityType.Other
    }

    hf quality(quality) codec(codec)
  }

  def createMovie (name: String, filename: String, year: Long, inf: String) = {
    val movie = HypeMovie.createMovie (name)
    val hf = (HypeFile create) movie(movie) title(name) filename(filename) full_info(inf)

    val codec = inf match {
      case CodecH264RE(_) => CodecType.H264
      case CodecXviDRE(_) => CodecType.XviD
      case _ => CodecType.Other
    }
    
    val quality = inf match {
      case QualitySDRE(_) => QualityType.SD
      case QualityHDRE(_) => QualityType.HD
      case QualityFullHDRE(_) => QualityType.FullHD
      case _ => QualityType.Other
    }

    hf quality(quality) codec(codec)
  }

  def createFile (name: String, filename: String, season: Long, episode: Long, inf: String): HypeFile = {
    val ep = HypeEpisode.createEpisode (name, season, episode)
    val hf = (HypeFile create) episode(ep) filename(filename) full_info(inf)

    val codec = inf match {
      case CodecH264RE(_) => CodecType.H264
      case CodecXviDRE(_) => CodecType.XviD
      case _ => CodecType.Other
    }
    
    val quality = inf match {
      case QualitySDRE(_) => QualityType.SD
      case QualityHDRE(_) => QualityType.HD
      case QualityFullHDRE(_) => QualityType.FullHD
      case _ => QualityType.Other
    }

    hf quality(quality) codec(codec)
  }

  def update () {
    val dir = Props.get ("library.path") openOr "/home/fx/Videos/"
    info ("Upating directory structure: " ++ dir)

    val fslist = Path (new java.io.File (dir)).walk map (_.path replaceAll("""^""" + dir, "")) filter(f => !(f matches("""(?i).*sample.*"""))) filter(_ match { case ExtRE(ext) => true; case _ => false }) toList
    val fulldblist = HypeFile.findAll ()
    val dblist = fulldblist filter(d => fslist contains d.filename.is) map (_.filename)

    // Remove all shows that aren't in the file system anymore
    fulldblist filter(d => !(fslist contains d.filename.is)) foreach (_ delete_!)

    // Remove all episodes that don't have any file left
    HypeEpisode.findAll() filter(d => d.files.length <= 0) foreach (_ delete_!)

    // Parse new files for shows
    fslist
      .filter (f => !(dblist contains f))
      .map (_ match {
        case n@AnimeRE (dir, source, _, name, _, _, episode, inf) => HypeFile.createFile (name clean, n, 1, episode toLong, inf)
        case n@Anime2RE (dir, source, _, name, _, episode, inf) => HypeFile.createFile (name clean, n, 1, episode toLong, inf)
        case n@Anime3RE (dir, source, name, episode, inf) => HypeFile.createFile (name clean, n, 1, episode toLong, inf)
        case n@Anime4RE (dir, source, name, episode, inf) => HypeFile.createFile (name clean, n, 1, episode toLong, inf)
        case n@TVRE (dir, name, season, episode, inf) => HypeFile.createFile (name clean, n, season toLong, episode toLong, inf)
        case n@TV2RE (dir, name, season, episode, inf) => HypeFile.createFile (name clean, n, season toLong, episode toLong, inf)
        case n@MovieRE (dir, name, year, inf) => HypeFile.createMovie (name clean, n, year toLong, inf)
        case n@_ => HypeFile.createMisc (n)
      }
    ).foreach (_ save)
  }
}
