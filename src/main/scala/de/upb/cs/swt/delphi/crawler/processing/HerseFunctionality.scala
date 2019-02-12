package de.upb.cs.swt.delphi.crawler.processing

import java.io.BufferedInputStream

import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage
import de.upb.cs.swt.delphi.crawler.tools.JSFilesHandler

trait HerseFunctionality extends JSFilesHandler{

  def computeHerseMetrics(p: NpmPackage): HerseResults = {

    createJsRepo(p.identifier.toString,new BufferedInputStream(p.zipFile.is))
    val feature = Map("Ploc" -> 50)

    new HerseResults(p.identifier,feature)


  }

}
