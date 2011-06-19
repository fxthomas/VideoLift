package code
package model

import net.liftweb._
import net.liftweb.util.Props
import net.liftweb.mapper._
import net.liftweb.common._

import trakt._

class HypeShow extends LongKeyedMapper[HypeShow] with OneToMany[Long, HypeShow] {
  def getSingleton = HypeShow
  def primaryKeyField = id
  
  object id extends MappedLongIndex (this)
  object title extends MappedString (this, 256)
  object tvdb_id extends MappedLong (this)
  object description extends MappedText (this)
  object image extends MappedString (this, 256)
  object files extends MappedOneToMany (HypeEpisode, HypeEpisode.show, OrderBy(HypeEpisode.season, Ascending), OrderBy(HypeEpisode.episode, Ascending))
}

object HypeShow extends HypeShow with LongKeyedMetaMapper[HypeShow] with Logger {
  override def dbTableName = "shows"
  
  def createShow (name: String): HypeShow = {
    val hf = (HypeShow create) title (name)

    info ("Searching Show: " ++ name)
    val shows = try Show.search(name) catch {
      case e:java.io.IOException => { info (e); List() }
      case e:net.liftweb.json.JsonParser.ParseException => { info (e); List() }
    }
    {if (shows.length > 0) {
      info ("Show found: " ++ shows(0).title)
      if (shows(0).title != name) (HypeMapper create) original(name) map(shows(0).tvdb_id toInt) save

      hf title(shows(0).title)
      hf description (shows(0).overview)
      hf tvdb_id (shows(0).id)
      hf image (shows(0).images.poster)
    } else { info ("Not found: " ++ name); hf }} save

    hf
  }

  def createShow (name: String, tvdb_id: Long): HypeShow = {

    info ("Searching Show: " + name + " with TVDB ID: " + tvdb_id)
    val newshow = {try {
      val hf = (HypeShow create) title (name)
      val sh = Show.select(tvdb_id)
      info ("Show found: " + sh.title)
      hf title(sh.title)
      hf description (sh.overview)
      hf tvdb_id (sh.id)
      hf image (sh.images.poster)
    } catch {
      case e:java.io.IOException => { info (e); (HypeShow create) title(name) }
      case e:net.liftweb.json.JsonParser.ParseException => { info (e); (HypeShow create) title(name) }
    }}

    newshow save;
    newshow
  }
}
