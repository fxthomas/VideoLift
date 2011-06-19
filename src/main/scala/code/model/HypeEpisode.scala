package code
package model

import net.liftweb._
import util.Props
import mapper._
import common._

import trakt._

class HypeEpisode extends LongKeyedMapper[HypeEpisode] with OneToMany[Long, HypeEpisode] {
  def getSingleton = HypeEpisode
  def primaryKeyField = id

  object id extends MappedLongIndex (this)
  object title extends MappedString (this, 256)
  object files extends MappedOneToMany (HypeFile, HypeFile.episode, OrderBy(HypeFile.quality, Ascending))
  object imdb_id extends MappedLong (this)
  object show extends LongMappedMapper (this, HypeShow)
  object season extends MappedLong (this)
  object episode extends MappedLong (this)
  object description extends MappedText (this)
  object image extends MappedString (this, 256)
}

object HypeEpisode extends HypeEpisode with LongKeyedMetaMapper[HypeEpisode] with Logger {
  override def dbTableName = "episodes"

  def createEpisode (_name: String, season: Long, episode: Long): HypeEpisode = {
    info ("Searching show: " + _name + " (Season = " + season + ", Episode = " + episode + ")")

    // Find show filename mapping if there is one
    val mapping = HypeMapper.findAll (By (HypeMapper.original, _name))
    val show = if (mapping.length > 0) {
      val hs = HypeShow.findAll (By (HypeShow.tvdb_id, mapping(0).map.is))
      if (hs.length > 0) hs(0) else { HypeShow.createShow(_name, mapping(0).map.is) }
    } else {
      val hs = HypeShow.findAll (By (HypeShow.title, _name))
      if (hs.length > 0) hs(0) else { HypeShow.createShow(_name) }
    }

    // Search for episode
    val ep = HypeEpisode.findAll (By(HypeEpisode.show, show), By(HypeEpisode.season, season), By(HypeEpisode.episode, episode))
    if (ep.length > 0) ep(0)
    else {
      // Create Episode
      val hf = (HypeEpisode.create) show(show) season(season) episode(episode);

      // Search for episode
      Episode.summary (show.tvdb_id, season, episode) match {
        case Full(eps) => { hf description(eps.episode.overview); hf image(eps.episode.images.screen); hf title(eps.episode.title) }
        case _ => hf description("No description")
      }
      hf save;
      hf
    }
  }
}
