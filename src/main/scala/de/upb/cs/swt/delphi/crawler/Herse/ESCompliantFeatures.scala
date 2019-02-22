package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s.JsonAST.{JField, JString, JValue}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ESCompliantFeatures(ast: JValue) extends HerseFeatures {



  def checkES2018Compliant = {
    ast findField{
      case JField("name", JString("finally")) => true
      case _ => false
    } match {
      case Some(value) =>  ES2018Compliant = true

      case None =>
    }

    ast findField{
      case JField("pattern",_) => true
      case _ => false
    } match {
      case Some(value) => if(value._2.values.asInstanceOf[String].contains("?<=")) {
        ES2018Compliant = true

      }
      case None =>
    }



  }

  def checkES2017Compliant =  {

    ast findField{
      case JField("name",_) => true
      case _ => false
    } match {
      case Some(value) =>  if (value._2.values.asInstanceOf[String].contains("padStart") || value._2.values.asInstanceOf[String].contains("padEnd"))

        ES2017Compliant = true

      case None =>


    }
    (ast \\ "expression" \\ "object").toOption match {
      case Some(value) => (ast \\ "expression" \\ "property").toOption match {
        case Some(prop) => prop findField{
          case JField("name", _) => true
          case _=> false
        } match {
          case Some(feature) =>  if (feature._2.values.asInstanceOf[String].contains("values") || feature._2.values.asInstanceOf[String].contains("getOwnPropertyDescriptors") ||
            feature._2.values.asInstanceOf[String].contains("entries")) {
            ES2017Compliant = true

          }
          case None =>
        }
        case None =>
      }
      case None => println(ast)
    }

    ast findField{
      case JField("type" , JString("AwaitExpression")) => true
      case  _ => false
    } match {
      case Some(value) => ES2017Compliant = true


      case None =>
    }




  }

  def checkES2016Compliant =  {

    (ast \\ "property").toOption match {
      case Some(prop) =>
        prop findField{
          case JField("name" , JString("includes")) => true
          case  _ => false
        } match {
          case Some(value) =>
            ES2016Compliant = true

          case None =>
        }
      case None =>
    }

    ast findField {
      case JField("operator" , JString("**")) => true
      case _ => false
    } match {
      case Some(value) => println(value)
        ES2016Compliant = true
      case None =>

    }


  }

  def checkES2015Compliant = {

    ast findField {
      case JField("kind",_) => true
      case _ => false
    } match {
      case Some(value) => if(value._2.values.asInstanceOf[String].contains("const") || value._2.values.asInstanceOf[String].contains("let"))

        ES2015Compliant = true
      case None =>
    }

    ast findField {
      case JField("type", _) => true
      case _ => false
    } match {
      case Some(value) => if(value._2.values.asInstanceOf[String].contains("ArrowFunctionExpression") || value._2.values.asInstanceOf[String].contains("TemplateLiteral") ||
        value._2.values.asInstanceOf[String].contains("ForOfStatement") )
        ES2015Compliant = true
      case None =>
    }

    (ast \\ "expression" \\ "object").toOption match {
      case Some(value) => (ast \\ "expression" \\ "property").toOption match {
        case Some(prop) => prop findField{
          case JField("name", _) => true
          case _=> false
        } match {
          case Some(feature) =>  if (feature._2.values.asInstanceOf[String].contains("is") || feature._2.values.asInstanceOf[String].contains("setPrototypeOf") ||
            feature._2.values.asInstanceOf[String].contains("assign")) {
            ES2017Compliant = true

          }
          case None =>
        }
        case None =>
      }
      case None =>
    }



  }

  def checkESCompliant : Future[Map[String,Boolean]] = Future {

    checkES2015Compliant
    checkES2016Compliant
    checkES2017Compliant
    checkES2018Compliant
    Map("ES2016Compliant" -> ES2016Compliant , "ES2017Compliant" -> ES2017Compliant,
      "ES2018Compliant" -> ES2018Compliant, "ES2015Compliant" -> ES2015Compliant)
  }

}
