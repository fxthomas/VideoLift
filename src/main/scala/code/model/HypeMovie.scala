package code
package model

import net.liftweb._
import util.Props
import mapper._
import common._

import trakt._

class HypeMovie extends LongKeyedMapper[HypeMovie] with OneToMany[Long, HypeMovie] {
  def getSingleton = HypeMovie
  def primaryKeyField = id

  object id extends MappedLongIndex (this)
  object year extends MappedLong (this)
  object title extends MappedString (this, 256)
  object files extends MappedOneToMany (HypeFile, HypeFile.movie, OrderBy(HypeFile.quality, Ascending))
  object imdb_id extends MappedString (this, 50)
  object description extends MappedText (this)
  object image extends MappedString (this, 256)
  object duration extends MappedLong (this)
}

object HypeMovie extends HypeMovie with LongKeyedMetaMapper[HypeMovie] with Logger {
  override def dbTableName = "movies"

  def createMovie (name: String): HypeMovie = {
    info ("Searching movie: " + name)

    // Find filename mapping if there is one
    val mapping = HypeMapper.findAll (By (HypeMapper.original, name))
    val movies = if (mapping nonEmpty) HypeMovie.findAll (By (HypeMovie.imdb_id, mapping(0).map.is))
                 else HypeMovie.findAll (By (HypeMovie.title, name))

    // Search for movie
    if (movies nonEmpty) movies(0)
    else {
      // Create movie
      val hf = HypeMovie.create

      // Search for movie
      val movlist = Movie.search(name)

      if (movlist nonEmpty) {
        if (name != movlist(0).title) {
          (HypeMapper create) original(name) map(movlist(0).imdb_id) save
        }

        hf imdb_id(movlist(0).imdb_id)
        hf title(movlist(0).title)
        hf year(movlist(0).year)
        hf description(movlist(0).overview)
        hf image(movlist(0).images.poster)
        hf duration(movlist(0).runtime)
      } else {
        hf title(name)
      }

      // Save movie
      hf save;
      hf
    }
  }
}
