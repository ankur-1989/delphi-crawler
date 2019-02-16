/**
  * @author Ankur Gupta
  *         This class is to compute the metrics on javascript projects.
  */



package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s._
import org.json4s.jackson.JsonMethods
import scala.language.dynamics
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



class HerseAnalyzer(jsonAst: String) extends HerseFeatures with Dynamic {

  implicit val formats = DefaultFormats

  def computeCountComments : Future[Map[String,Int]] = Future {

      val jsonObject = JsonMethods.parse(jsonAst)
    (jsonObject \ "comments").toOption match {
      case Some(jArrComments) =>  val comments =  getElement("type",jArrComments)
                                   comments.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("Line") match {
                                     case Some(value) => SingleLineComments = value
                                     case None =>
                                   }

        comments.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("Block") match {
          case Some(value) => MultiLineComments = value
          case None =>
        }

      case None =>
    }

      val results =  Map("SingleLineComments" -> SingleLineComments, "MultiLineComments" -> MultiLineComments)
      results
  }

  def computeLOC(sourceFile: String) : Future[Map[String, Int]] = Future {

        val sourceCode = scala.io.Source.fromFile(sourceFile).mkString
        val result = Map("Ploc" -> sourceCode.count(_ == '\n'))
    result
  }



  def getElement(elem: String , json: JValue) = for {
    JObject(child) <- json
    JField(`elem`,JString(value)) <-  child
  } yield value

}
