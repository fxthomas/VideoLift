package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

import HypeShow._

class HypeMapper extends LongKeyedMapper[HypeMapper] {
  def getSingleton = HypeMapper // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex (this)
  object original extends MappedString (this, 256)
  object map extends MappedLong (this)
}

object HypeMapper extends HypeMapper with LongKeyedMetaMapper[HypeMapper] {
  override def dbTableName = "mapper" // define the DB table name
}
