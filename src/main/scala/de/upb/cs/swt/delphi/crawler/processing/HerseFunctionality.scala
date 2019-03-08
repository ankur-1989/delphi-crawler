package de.upb.cs.swt.delphi.crawler.processing

import java.io.BufferedInputStream

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.Herse.HerseCore
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage
import de.upb.cs.swt.delphi.crawler.tools.JSFilesHandler

trait HerseFunctionality extends JSFilesHandler {


  def computeHerseResults(p: NpmPackage)(implicit log: LoggingAdapter): HerseResults = {


    var results = Map[String, Any]()


      try{
        createJsRepo(p.identifier.toString, new BufferedInputStream(p.zipFile.is))

        log.info(s"SOURCE FILE ${p.identifier.toString}")

        val sourceFile = getTargetFile(p.identifier.toString)



        if (!sourceFile.isEmpty) {


          results = HerseCore.computeJSMetrics(sourceFile)

        }
      } catch {
        case unknownError: UnknownError => log.error(s"${unknownError.printStackTrace()}")
          deleteJsRepo(p.identifier.toString)
      } finally {
        deleteJsRepo(p.identifier.toString)
      }





    new HerseResults(p.identifier, results)

  }

}
