/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.Herse

import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.escalima.ast.UpdateOperator.`++`
import de.upb.cs.swt.delphi.crawler.processing
import org.json4s
import org.json4s.JString
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.JsonMethods.{compact, parse, render}

import scala.language.dynamics
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await
import org.json4s._

import sys.process._
object HerseCore {

  def computeJSMetrics(sourceFile: String)(implicit log: LoggingAdapter): Map[String, Any] = {


    val loc = true
    var astComments = (s"node ${processing.parserScript} ${sourceFile} ${loc}".!!).toString
    var jsonAstWithComments = parse(astComments)


    var jsonAST = (s"node ${processing.parserScript} ${sourceFile} false".!!).toString

    var jsonObject = parse(jsonAST)

    // Transforming the ASTs to handle the special cases of braces as raw values found in different keys of JsonAST
    // such as inside raw , patterns and values keys

    jsonObject = jsonObject transformField {
      case JField("raw", JString(s)) => ("raw", JString(s.replaceAll("\\}", ")")))
      case JField("value", JString(s)) => ("value", JString(s.replaceAll("\\}", ")")))
      case JField("pattern", JString(s)) => ("pattern", JString(s.replaceAll("\\}", ")")))
    }

    jsonObject = jsonObject transformField {

      case JField("raw", JString(s)) => ("raw", JString(s.replaceAll("\\{", "(")))
      case JField("value", JString(s)) => ("value", JString(s.replaceAll("\\{", "(")))
      case JField("pattern", JString(s)) => ("pattern", JString(s.replaceAll("\\{", "(")))
    }

    jsonAstWithComments = jsonAstWithComments transformField {
      case JField("raw", JString(s)) => ("raw", JString(s.replaceAll("\\}", ")")))
      case JField("value", JString(s)) => ("value", JString(s.replaceAll("\\}", ")")))
      case JField("pattern", JString(s)) => ("pattern", JString(s.replaceAll("\\}", ")")))

    }

    jsonAstWithComments = jsonAstWithComments transformField {
      case JField("raw", JString(s)) => ("raw", JString(s.replaceAll("\\{", "(")))
      case JField("value", JString(s)) => ("value", JString(s.replaceAll("\\{", "(")))
      case JField("pattern", JString(s)) => ("pattern", JString(s.replaceAll("\\{", "(")))
    }

    astComments = compact(render(jsonAstWithComments))

    jsonAST = compact(render(jsonObject))

    val createObject = new CreateObjectMaps(jsonAST, sourceFile, astComments)
    createObject.createOpeningIndexesMap
    createObject.createClosingIndexMap

    val asyncMetrics = new AsynchronousMetrics(jsonAST, sourceFile, jsonObject, createObject, astComments)
    val classMetrics = new JsClassMetrics(jsonAST, jsonObject, createObject)
    val regExMetrics = new RegExMetrics(jsonObject)

    val dynamicMetric = new DynamicMetrics(jsonAST, jsonObject, createObject)
    val jsmoduleType = new JsModuleFeatures(sourceFile, jsonObject, jsonAST)
    val analyzer = new HerseAnalyzer(jsonAST, sourceFile, jsonObject, createObject, astComments)
    val ff = new FanInFanout(jsonAST, createObject)
    val es = new ESCompliantFeatures(jsonObject)
    val obj = new ObjectFeatures(jsonAST, createObject)

    val features = for {
      complexityFeatures <- analyzer.complexityMetric
      esFeatures <- es.checkESCompliant
      fanInfanOut <- ff.computeFFMetrics
      objectMetric <- obj.computeObjectMetrics
      jsdynamicfeatures <- dynamicMetric.computeDynamicMetrics
      jsModuleFeatures <- jsmoduleType.checkModuleType
      asyncFeatures <- asyncMetrics.computeAsyncMetrics
      classFeatures <- classMetrics.computeClassMetrics
      regexFeatures <- regExMetrics.computeRegExMetrics
    } yield (complexityFeatures, esFeatures, fanInfanOut, objectMetric,
      jsdynamicfeatures, jsModuleFeatures, asyncFeatures, classFeatures, regexFeatures)

    val HerseFeatures = Await.result(features, 10.hours)

    println(HerseFeatures._1 ++ HerseFeatures._2 ++ HerseFeatures._3 ++ HerseFeatures._4 ++ HerseFeatures._5 ++ HerseFeatures._6 ++ HerseFeatures._7
      ++ HerseFeatures._8 ++ HerseFeatures._9)

    HerseFeatures._1 ++ HerseFeatures._2 ++ HerseFeatures._3 ++ HerseFeatures._4 ++ HerseFeatures._5 ++ HerseFeatures._6 ++ HerseFeatures._7 ++ HerseFeatures._8 ++ HerseFeatures._9

  }
}



