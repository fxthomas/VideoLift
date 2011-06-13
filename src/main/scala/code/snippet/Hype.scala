package code
package snippet

import scala.xml.{NodeSeq, Text}

import net.liftweb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props 
import net.liftweb.http.{ SHtml, IdMemoizeTransform }
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.{OrderBy, Ascending}

import model.HypeFile

object Hype {
  def findItems = HypeFile.findAll(OrderBy (HypeFile.show, Ascending), OrderBy (HypeFile.season, Ascending), OrderBy (HypeFile.episode, Ascending))
  var items = findItems
}

class Hype {
  def renderList = ".file *" #> Hype.items.map (d =>
    (if (d.title.is != "") ".title *" #> d.title else ".title" #> "") &
    ".description *" #> (".episode *" #> d.description) &
    (if (d.season.is != 0) ".season *" #> ("Season " + d.season) else ".season" #> "") &
    (if (d.episode.is != 0) ".episode *" #> ("Episode " + d.episode) else ".episode" #> "") &
    ".picture *" #> (
      ".download [href]" #> ((Props.get("library.url") openOr "") + d.filename)
    ) &
    (d.show.obj match {
      case Full(s) => {
        ".showtitle *" #> s.title &
        ".description *" #> (".series *" #> s.description) &
        (if (s.image.is != "") ".picture" #> ("img [src]" #> s.image)
        else ".picture *" #> ("img" #> "" ))
      }
      case _ => {
        ".showtitle" #> "" &
        ".description *" #> (".series" #> "") &
        ".picture *" #> ("img" #> "")
      }
    }))

  def render = "*" #> renderList &
    "*" #> SHtml.idMemoize (outer =>
      "#reload [onclick]" #> SHtml.ajaxInvoke (() => { HypeFile.update(); Hype.items = Hype.findItems; SetHtml (outer.latestId, renderList(outer.applyAgain())) })
    )
}
