package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.{JNothing, JNull, JsonAST}
import org.json4s.JsonAST.{JObject, JString, JValue}


trait AstTraverse extends HerseFeatures {


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


}
