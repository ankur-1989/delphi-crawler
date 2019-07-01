package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s.JsonAST.{JField, JString, JValue}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ESCompliantFeatures(ast: JValue) extends HerseFeatures {



  def checkES2019Compliant = {

    (ast \\ "expression" \\ "property").toOption match {
      case Some(prop) => prop findField {

        case JField("name", JString("flat")) => true
        case JField("name", JString("flatMap")) => true
        case JField("name", JString("fromEntries")) => true
        case JField("name", JString("trimStart")) => true
        case JField("name", JString("trimEnd")) => true
        case JField("name", JString("description")) => true
        case _ => false
      } match {
        case Some(feature) => {

          ES2019Compliant = 1
          ES2018Compliant = 1
          ES2017Compliant = 1
          ES2016Compliant = 1
          ES2015Compliant = 1

        }
        case None =>
      }
      case None =>
    }
  }

  def checkES2018Compliant = {
    ast findField {
      case JField("name", JString("finally")) => true
      case _ => false
    } match {
      case Some(value) => ES2018Compliant = 1
        ES2017Compliant = 1
        ES2016Compliant = 1
        ES2015Compliant = 1

      case None =>
    }

    if (ES2018Compliant == 0) {
      ast findField {
        case JField("pattern", _) => true
        case _ => false
      } match {
        case Some(value) => if (value._2.values.asInstanceOf[String].contains("?<=")) {
          ES2018Compliant = 1
          ES2017Compliant = 1
          ES2016Compliant = 1
          ES2015Compliant = 1

        }
        case None =>
      }
    }
  }

  def checkES2017Compliant = {

    ast findField{
      case JField("name",JString("padStart")) => true
      case JField("name",JString("padEnd")) => true
      case JField("type" , JString("AwaitExpression")) => true


      case _ => false
    } match {
      case Some(value) => {

        ES2017Compliant = 1
        ES2016Compliant = 1
        ES2015Compliant = 1
      }

      case None =>


    }
    if(ES2017Compliant == 0) {
      (ast \\ "expression" \\ "object").toOption match {
        case Some(value) => (ast \\ "expression" \\ "property").toOption match {
          case Some(prop) => prop findField {

            case JField("name", JString("getOwnPropertyDescriptors")) => true
            case JField("name", JString("entries")) => true
            case JField("name", JString("values")) => true

            case _ => false
          } match {
            case Some(feature) => {
              ES2017Compliant = 1
              ES2016Compliant = 1
              ES2015Compliant = 1

            }
            case None =>
          }
          case None =>
        }
        case None =>
      }
    }





  }

  def checkES2016Compliant = {

    (ast \\ "property").toOption match {
      case Some(prop) =>
        prop findField {
          case JField("name", JString("includes")) => true
          case _ => false
        } match {
          case Some(value) =>

            ES2016Compliant = 1
            ES2015Compliant = 1

          case None =>
        }
      case None =>
    }
    if (ES2016Compliant == 0){
      ast findField {
        case JField("operator", JString("**")) => true
        case _ => false
      } match {
        case Some(value) =>
          ES2016Compliant = 1
          ES2015Compliant = 1

        case None =>

      }
    }

  }

  def checkES2015Compliant = {

    ast findField {
      case JField("kind", JString("const")) => true
      case JField("kind", JString("let")) => true
      case JField("type", JString("ArrowFunctionExpression")) => true
      case JField("type", JString("TemplateLiteral")) => true
      case JField("type", JString("ForOfStatement")) => true
      case _ => false
    } match {
      case Some(value) =>
        ES2015Compliant = 1
      case None =>
    }


    if(ES2015Compliant == 0) {
      (ast \\ "expression" \\ "object").toOption match {
        case Some(value) => (ast \\ "expression" \\ "property").toOption match {
          case Some(prop) => prop findField {
            case JField("name", JString("is")) => true
            case JField("name", JString("setPrototypeOf")) => true
            case JField("name", JString("assign")) => true
            case _ => false
          } match {
            case Some(feature) =>  {
              ES2015Compliant = 1

            }
            case None =>
          }
          case None =>
        }
        case None =>
      }
    }

  }

  def checkESCompliant : Future[Map[String,Any]] =  Future {

    checkES2015Compliant
    if(ES2015Compliant == 1) { checkES2016Compliant }
    if(ES2016Compliant == 1) {checkES2017Compliant}
    if(ES2017Compliant == 1 ) {checkES2018Compliant}
    if(ES2018Compliant == 1 ) {checkES2019Compliant}

    Map("ES2017Compliant" -> ES2017Compliant , "ES2018Compliant" -> ES2018Compliant , "ES2016Compliant" -> ES2016Compliant , "ES2015Compliant" -> ES2015Compliant,
      "ES2019Compliant" -> ES2019Compliant)

  }

}
