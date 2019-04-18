package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.JValue
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.jackson.JsonMethods.parse

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class JsClassMetrics(ast: String, jsonObject : JValue, createObjectMaps: CreateObjectMaps) extends  HerseFeatures {


  var methodsPerClass : List[Int] = List()

  def computeClassDeclarations = {

    NoOfClassDeclarations  = createObjectMaps.classDeclarationMap.size

  }

  def computeMethodsPerClass = {

    for ((key,value) <- createObjectMaps.classDeclarationIndexMap) {

      val methodList : List[String] =  for {
        JObject(child) <- parse(ast.substring(key,value+1))
        JField("type", JString(value)) <- child
        if(value.equals("MethodDefinition"))
      } yield value

      if (methodList.size  > 0) {
        methodsPerClass = methodList.groupBy(identity).mapValues(_.size).get("MethodDefinition").get :: methodsPerClass
      }

    }

    if(methodsPerClass.size > 0 ) {

      HighestNoOfMethodsPerClass = methodsPerClass.iterator.max
      AvgNoOfMethodsPerClass = BigDecimal(methodsPerClass.iterator.reduceLeft(_+_).toDouble / methodsPerClass.size.toDouble).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble

    }


  }

  def computeClassMetrics : Future[Map[String,Any]] = Future {

    computeClassDeclarations
    computeMethodsPerClass



    Map("NoOfClassDeclarations" -> NoOfClassDeclarations , "HighestNoOfMethodsPerClass" -> HighestNoOfMethodsPerClass , "AvgNoOfMethodsPerClass" ->
      AvgNoOfMethodsPerClass)

  }

}

