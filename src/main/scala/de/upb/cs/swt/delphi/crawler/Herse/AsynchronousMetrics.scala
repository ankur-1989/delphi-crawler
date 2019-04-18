package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.JsonAST.{JField, JObject, JString, JValue}
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.JsonMethods._

import sys.process._
import scala.collection.immutable.{ListMap, Range}
import scala.language.dynamics
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.{BigDecimal, log10}


class AsynchronousMetrics(jsonAst: String, sourceFile: String, jsonObject: JValue, createObjectMaps: CreateObjectMaps,astComments: String) extends HerseFeatures {


  def computeAwaitExpressions = {

    val awaitExpressionList: List[String] = for {
      JObject(child) <- jsonObject
      JField("type", JString(value)) <- child
      if (value.equals("AwaitExpression"))
    } yield value

    awaitExpressionList.groupBy(identity).mapValues(_.size).get("AwaitExpression") match {
      case Some(value) => NoOfAwaitExpressions = value
      case None =>
    }



  }

  def computeAsyncFuncorMethods = {


    val asyncMethodsList: List[Boolean] = for {
      JObject(child) <- jsonObject
      JField("async", JBool(value)) <- child
      if (value == true)
    } yield value


    asyncMethodsList.groupBy(identity).mapValues(_.size).get(true) match {
      case Some(value) => NoOfAsyncFunctions = value
      case None =>
    }



  }

  def computeTimerStatements = {

    (jsonObject \\ "callee" \\ "name").toOption match {
      case Some(JObject(nameList)) =>
        NoOfTimerStatements = nameList.filter( f => f._2.values.equals("setTimeout") || f._2.values.equals("setInterval") ||
          f._2.values.equals("clearTimeout") || f._2.values.equals("clearInterval") || f._2.values.equals("setImmediate") || f._2.values.equals("clearImmediate")).size
      case None =>
    }

  }

  def computeAsyncMetrics : Future[Map[String,Any]] =Future {

    computeAwaitExpressions
    computeAsyncFuncorMethods

    Map("NoOfAwaitExpressions" -> NoOfAwaitExpressions , "NoOfAsyncFunctions" -> NoOfAsyncFunctions , "NoOfTimerStatements" -> NoOfTimerStatements)

  }

}
