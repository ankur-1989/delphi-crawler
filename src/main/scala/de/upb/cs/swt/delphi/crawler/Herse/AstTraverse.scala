package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s
import org.json4s.{JArray, JNothing, JNull, JValue, JsonAST}
import org.json4s.JsonAST.{JField, JObject, JString, JValue}


trait AstTraverse extends HerseFeatures {


  var listUniqueOperands : List[String] = List()

  def checkParams(node: JValue) : Any = {

    node match {
      case JString(s) => s
      case JNull =>
      case JNothing =>
      case JObject(obj) => if(obj.nonEmpty)  obj.foreach(f => if(f._1.contains("params")) LargestSignatureInFunction = if(f._2.children.size > LargestSignatureInFunction) f._2.children.size else LargestSignatureInFunction
      else checkParams(f._2))
      case JsonAST.JArray(arr) =>  if(arr.nonEmpty) arr.foreach(f => checkParams(f))
      case JsonAST.JBool(value) => value
      case JsonAST.JDecimal(num) => num
      case JsonAST.JInt(num) => num
    }

  }

  def getElement(elem: String , json: JValue) = for {
    JObject(child) <- json
    JField(`elem`,JString(value)) <-  child
  } yield value


  def getVariables(elem: String , json: JValue) = for {
    JObject(child) <- json
    JField(`elem`,JArray(arr)) <-  child
  } yield arr


}
