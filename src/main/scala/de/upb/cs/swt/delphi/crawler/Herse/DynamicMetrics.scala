package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.JsonAST._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DynamicMetrics(ast : String, jsonObject : JValue, createObjectMaps: CreateObjectMaps) extends AstTraverse with HerseFeatures {



  def computeReflectOperators() ={

    val reflectOperatorsList: List[String] =  for {
      JObject(child) <- jsonObject
      JField("operator", JString(value)) <- child
      if (value.equals("instanceof") || value.equals("typeof"))
    } yield value

    reflectOperatorsList.groupBy(identity).mapValues(_.size).get("typeof") match {
      case Some(value) => NoOfTypeOfOperator = value
      case None =>
    }

    reflectOperatorsList.groupBy(identity).mapValues(_.size).get("instanceof") match {
      case Some(value) => NoOfInstanceOfOperator = value
      case None =>
    }


  }

  def computeReflectMethods = {

    val reflectMethodList : List[String] = for {
      JObject(child) <- jsonObject
      JField("name", JString(value)) <- child
      if (value.equals("Reflect"))
    } yield value

    reflectMethodList.groupBy(identity).mapValues(_.size).get("Reflect") match {
      case Some(value) => NoOfReflectMethods = value
      case None =>
    }

  }

  def computeGeneratorFunctions() = {

    val generatorFunctionList : List[Boolean] = for {
      JObject(child) <- jsonObject
      JField("generator", JBool(value)) <- child
      if (value == true)
    } yield value


    NoOfGeneratorFunctions = generatorFunctionList.size

  }

  def computeJavaScriptConstructs = {

    val withStatementList : List[String] = for {
      JObject(child) <- jsonObject
      JField("type", JString(value)) <- child
      if (value.equals("WithStatement"))
    } yield value

    withStatementList.groupBy(identity).mapValues(_.size).get("WithStatement") match {
      case Some(value) =>  NoOfWithStatementUsages = value
      case None =>
    }

    val evalConstructList : List[String] = for {
      JObject(child) <- jsonObject
      JField("name", JString(value)) <- child
      if (value.equals("eval"))
    } yield value

    evalConstructList.groupBy(identity).mapValues(_.size).get("eval") match {
      case Some(evalcount) => NoOfEvalUsages = evalcount
      case None =>
    }
  }

  def computeDynamicMetrics :Future[Map[String,Any]] =  Future {


    computeReflectOperators
    computeReflectMethods
    computeGeneratorFunctions
    computeJavaScriptConstructs

    Map("NoOfInstanceOfOperator" -> NoOfInstanceOfOperator , "NoOfTypeOfOperator" -> NoOfTypeOfOperator , "NoOfReflectMethods" -> NoOfReflectMethods
      , "NoOfGeneratorFunctions" -> NoOfGeneratorFunctions , "NoOfEvalUsages" -> NoOfEvalUsages, "NoOfWithStatementUsages" -> NoOfWithStatementUsages )

  }



}

