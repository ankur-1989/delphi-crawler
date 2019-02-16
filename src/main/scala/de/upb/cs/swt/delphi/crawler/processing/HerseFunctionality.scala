package de.upb.cs.swt.delphi.crawler.processing

import java.io.BufferedInputStream

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.Herse.HerseCore
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage
import de.upb.cs.swt.delphi.crawler.tools.JSFilesHandler

trait HerseFunctionality extends JSFilesHandler{



  def computeHerseResults(p: NpmPackage)(implicit log: LoggingAdapter): HerseResults = {

    log.info("Starting Herse Metrics computation process")



    createJsRepo(p.identifier.toString,new BufferedInputStream(p.zipFile.is))



    val results = HerseCore.computeJSMetrics(getTargetFile(p.identifier.toString))

    deleteJsRepo(p.identifier.toString)

    new HerseResults(p.identifier,results)



  }

}
