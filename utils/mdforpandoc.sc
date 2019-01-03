// create markdown table suitable for processing with pandoc

import scala.io.Source
import edu.holycross.shot.cite._
import java.net.URI
import java.net.URL
import java.io.PrintWriter



case class Sns (urn: Cite2Urn, abbreviation: String, uri: URI, description: String, infoUrl: URL, status: String) {
  val validStatus = List("current", "deprecated")
  require(validStatus.contains(status), "Invalid value for status: " + status)
  //urn#abbreviation#uri#description#infourl#status

  def markdown : String = {
    s"## `${abbreviation}`\n\n" +
    s"URI: ${uri}\n\nStatus:  **${status}**\n\n" +
    s"${description}\n\n" +
    s"More information:  <u><${infoUrl}></u>\n\n"

  }
}


def snsFromCex(row: String): Sns = {
  val cols = row.split("#").toVector
  val urn = Cite2Urn(cols(0))
  val abbr = cols(1)
  val uri = new URI(cols(2))
  val descr = cols(3)
  val url = new URL(cols(4))
  val status = cols(5)
  Sns(urn, abbr, uri, descr, url, status)
}


def md (outFile : String, cexFile: String =  "server-subset/subnamespaces.cex") = {
  // read CEX file,  drop CEX block label, drop header line
  val cex = Source.fromFile(cexFile).getLines.toVector.tail.tail
  println("Read " + cex.size + " entries")
  val sns = for (row <- cex) yield {
    snsFromCex(row)
  }
  val mdText =   sns.sortBy(_.abbreviation).map(_.markdown).mkString
  new PrintWriter(outFile){write(mdText); close;}

}
