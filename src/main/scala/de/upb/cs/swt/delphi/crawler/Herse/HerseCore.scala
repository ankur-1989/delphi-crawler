/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.Herse

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.processing
import org.json4s.jackson.JsonMethods
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await
import sys.process._
object HerseCore {

  def computeJSMetrics(sourceFile: String)(implicit log: LoggingAdapter) : Map[String,Any] = {


   val jsonAST = s"node ${processing.parserScript} ${sourceFile}".!!

    val jsonObject = JsonMethods.parse(jsonAST)
    val analyzer = new HerseAnalyzer(jsonAST,sourceFile,jsonObject)

    val es= new ESCompliantFeatures(jsonObject)

   val features = for {
    complexityFeatures <- analyzer.complexityMetric
    esFeatures <- es.checkESCompliant
   } yield(complexityFeatures, esFeatures)


   val HerseFeatures = Await.result(features,40.seconds)

   println(HerseFeatures._1 ++ HerseFeatures._2)

   HerseFeatures._1 ++ HerseFeatures._2




  }


}
