package de.upb.cs.swt.delphi.crawler.Herse

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.processing

import scala.concurrent.duration._
import scala.concurrent.Await
import sys.process._
object HerseCore {

  def computeJSMetrics(sourceFile: String)(implicit log: LoggingAdapter) : Map[String,Int] = {




    val jsonAST = s"node ${processing.parserScript} ${sourceFile}".!!


    val analyzer = new HerseAnalyzer(jsonAST)

    val futureComments = analyzer.computeCountComments
    val futureLOC = analyzer.computeLOC(sourceFile)
    val futureFunctionsCount = analyzer.computeFunctionsCount

    val totalFeatures = futureFunctionsCount.zip(futureLOC.zip(futureComments))
    val features = Await.result(totalFeatures,10.seconds)

    log.info(s"Features MAP for ${sourceFile} ->  ${features._1 ++ features._2._1 ++ features._2._2}")
    features._1 ++ features._1 ++ features._2._1 ++ features._2._2


  }


}
