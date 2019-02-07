/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.preprocessing

import java.net.{URI, URL}

import de.upb.cs.swt.delphi.crawler.discovery.maven.HttpResourceHandler
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier

class NpmDownloader(identifier: NpmIdentifier) {

  val http = new HttpResourceHandler(constructPackageBaseUri())
  val gunZipResource = http.locate(gunZipFileName(identifier))

  /**
    * Construct url from npm identifier
    * @return Base URI
    */
  def constructPackageBaseUri(): URI =
    new URI(identifier.repository)
      .resolve(identifier.name + "/" + "-" + "/")


  def constructPackageUrl(): URL =
    constructPackageBaseUri().resolve(gunZipFileName(identifier)).toURL

  def gunZipFileName(identifier: NpmIdentifier) : String =
    identifier.name + "-" + identifier.version + ".tgz"

  def downloadGunZip(): GunZipFile = {
    GunZipFile(gunZipResource.read(), constructPackageUrl())
  }

}
