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
    val ff = new FanInFanout(jsonAST)
    val es= new ESCompliantFeatures(jsonObject)
    val obj = new ObjectFeatures(jsonAST)

   val features = for {
    complexityFeatures <- analyzer.complexityMetric
    esFeatures <- es.checkESCompliant
    fanInfanOut <-  ff.computeFanInFanOut
    objectMetric <- obj.computeObjectMetrics
   } yield(complexityFeatures, esFeatures,fanInfanOut,objectMetric)


   val HerseFeatures = Await.result(features,50.seconds)

   println(HerseFeatures._1 ++ HerseFeatures._2 ++ HerseFeatures._3 ++ HerseFeatures._4)

   HerseFeatures._1 ++ HerseFeatures._2 ++ HerseFeatures._3 ++ HerseFeatures._4




  }


}
