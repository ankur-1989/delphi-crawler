/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.discovery.npm

import de.upb.cs.swt.delphi.crawler.Identifier
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets

case class NpmIdentifier(val repository: String, val name: String, val version: String) extends Identifier {

   def toUniqueString = {
     repository + ":" + name + ":" + version
    }
  def toZipLocation : URI = {
     constructPackageBaseURI().resolve(encode(name) + "-" + encode(version) + ".tgz")

  }

  override val toString: String = name + ":" + version

  private def constructPackageBaseURI() : URI =  new URI(repository)
                                                  .resolve(encode(name) + "/")


  private def encode(input : String) : String =
    URLEncoder.encode(input, StandardCharsets.UTF_8.toString())



}
