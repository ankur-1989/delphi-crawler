package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.JsonAST.{JObject, JValue}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class RegExMetrics(jsonObject : JValue) extends HerseFeatures {

  def computeRegExUsageCount = {

    (jsonObject \\ "callee" \\ "name").toOption match {
      case Some(JObject(nameList)) =>
        NoOfRegExpressionUsages = nameList.filter( f => f._2.values.equals("RegExp")).size
      case None =>
    }

    (jsonObject \\ "regex").toOption match {
      case Some(JObject(obj)) =>
        NoOfRegExpressionUsages += obj.size
      case None =>
    }

  }

  def computeRegExMetrics : Future[Map[String,Any]] = Future {

    computeRegExUsageCount
    Map("NoOfRegExpressionUsages" -> NoOfRegExpressionUsages)
  }

}
