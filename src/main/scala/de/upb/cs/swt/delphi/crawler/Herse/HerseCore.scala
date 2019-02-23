/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.Herse

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.processing
import org.json4s.jackson.JsonMethods

import scala.concurrent.duration._
import scala.concurrent.Await
import sys.process._
object HerseCore {

  def computeJSMetrics(sourceFile: String)(implicit log: LoggingAdapter) : Map[String,Any] = {


   val jsonAST = s"node ${processing.parserScript} ${sourceFile}".!!

    val jsonObject = JsonMethods.parse(jsonAST)
    val analyzer = new HerseAnalyzer(jsonAST)
    val hm = new HalsteadMetrics(jsonObject)
    val es= new ESCompliantFeatures(jsonObject)
    val futureComments = analyzer.computeCountComments
    val futureLOC = analyzer.computeLOC(sourceFile)

    val futureFunctionsCount = analyzer.computeFunctionsCount
    val futureLargestSignature = analyzer.computeLargestSignature(jsonObject)
   val futureStatmentCount = analyzer.computeFunctionStatements

    val futureHalsteadMetrics = hm.computeHalsteadMetrics()



    val sizeFeatures  =    Await.result(futureLOC.zip(futureComments),15.seconds)
   val functionFeatures = Await.result(futureStatmentCount.zip(futureLargestSignature.zip(futureFunctionsCount)),15.seconds)
    val halsteadFeatures = Await.result(futureHalsteadMetrics,15.seconds)
    val esCompliantFeatures = Await.result(es.checkESCompliant,15.seconds)
    log.info(s"Features Map ->  ${sizeFeatures._2 ++ sizeFeatures._1 ++ functionFeatures._1 ++ functionFeatures._2._1 ++ functionFeatures._2._2 ++ halsteadFeatures ++ esCompliantFeatures}")
    sizeFeatures._2 ++ sizeFeatures._1 ++ functionFeatures._1 ++ functionFeatures._2._1 ++ functionFeatures._2._2 ++ halsteadFeatures ++ esCompliantFeatures



  }


}
