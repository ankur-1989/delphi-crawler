package de.upb.cs.swt.delphi.crawler.Herse

import java.io.File

import org.json4s.JsonAST.{JString, JValue}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class JsModuleFeatures(sourceFile : String, jsonObject : JValue , ast: String) extends HerseFeatures {

  def checkModuleType : Future[Map[String,Any]] = Future {


    var pattern = "require('.*)".r
    val code =  scala.io.Source.fromFile(new File(sourceFile),"ISO-8859-1").mkString

    pattern.findFirstIn(code) match {
      case Some(value) => CommonJsModuleType = 1
      case None => pattern = "module.exports".r
        pattern.findFirstIn(code) match {
          case Some(value) => CommonJsModuleType = 1

          case None =>
        }
    }

    jsonObject findField {
      case ("type", JString("ExportNamedDeclaration")) => true
      case ("type" , JString("ImportDeclaration")) => true
      case ("type", JString("ExportDefaultDeclaration")) => true
      case _ => false
    } match {
      case Some(value) => ES6ModuleType = 1
      case None =>
    }

    pattern = "define(.*)".r
    pattern.findFirstIn(code) match {
      case Some(value) =>
        AMDModuleType = 1
      case None =>
        if(code.indexOf("require(['") > 0) {

          AMDModuleType =1

        } else if(code.indexOf("curl(['") > 0 ) {
          AMDModuleType = 1
        }
    }

    pattern = "\\(function.*\\)".r
    pattern.findFirstIn(code) match {
      case  Some(value) =>
        pattern = "define(.*)".r
        pattern.findFirstIn(code) match {
          case Some(v) => UMDModuleType = 1
            CommonJsModuleType = 0
            AMDModuleType = 0
          case None => if(CommonJsModuleType == 1) {
            UMDModuleType = 1
            CommonJsModuleType = 0
            AMDModuleType = 0
          }
        }

      case None =>

    }
    Map("AMDModuleType" -> AMDModuleType , "CommonJsModuleType" -> CommonJsModuleType , "ES6ModuleType" -> ES6ModuleType , "UMDModuleType" -> UMDModuleType)
  }



}
