/**
  * @author Ankur Gupta
  *         This class is to compute the metrics on javascript projects.
  */



package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s._
import org.json4s.jackson.JsonMethods

import scala.collection.immutable.ListMap
import scala.language.dynamics
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



class HerseAnalyzer(jsonAst: String) extends HerseFeatures with Dynamic with AstTraverse {

  implicit val formats = DefaultFormats

  def computeCountComments : Future[Map[String,Any]] = Future {

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

  def computeLOC(sourceFile: String) : Future[Map[String, Any]] = Future {

        val sourceCode = scala.io.Source.fromFile(sourceFile).mkString
        val result = Map("Ploc" -> sourceCode.count(_ == '\n'))
    result
  }

  def computeFunctionsCount : Future[Map[String,Any]] = Future {

    val jsonObject = JsonMethods.parse(jsonAst)
    val functionsCount = getElement("type",jsonObject)
    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("FunctionDeclaration") match {
      case Some(value) => NoOfFunctionsDeclarations = NoOfFunctionsDeclarations + value
      case None =>
    }

    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("FunctionExpression") match {
      case Some(value) => NoOfFunctionsDeclarations = NoOfFunctionsDeclarations + value
      case None =>
    }

    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("ArrowFunctionExpression") match {
      case Some(value) => NoOfFunctionsDeclarations = NoOfFunctionsDeclarations + value
      case None =>
    }


    Map("NoOfFunctionsDeclarations" -> NoOfFunctionsDeclarations)

  }

  def computeLargestSignature(node: Any) : Future[Map[String,Any]]  = Future {


    checkParams(node.asInstanceOf[JValue])

    Map("LargestSignatureInFunction" -> LargestSignatureInFunction)

  }

  def computeFunctionStatements : Future[Map[String,Double]]   =  Future {


    getFunctionIndexes(jsonAst,"{\"type\":\"FunctionDeclaration\"")
    getFunctionIndexes(jsonAst,"{\"type\":\"FunctionExpression\"")
    getFunctionIndexes(jsonAst,"{\"type\":\"ArrowFunctionExpression\"")
    for((k,v) <- functionsMap) {
      findClosingIndex(jsonAst,k)
    }

    for((k,v) <- functionIndexMap) {
      getStatements(k,v,jsonAst.substring(k,v))
    }

    for((i,l) <- ListMap(functionStatementsMap.toSeq.sortBy(_._1):_*)) {


      var statementSize =   l.filter(f => f.contains("Statement") || f.contains("Declaration")).filter(f => !f.contains("Block")).groupBy(identity)
        .mapValues(_.size).foldLeft(0)(_+_._2)

      mapFunctionStatements = mapFunctionStatements ++ Map(i -> statementSize)


    }
    for ((s, c) <- mapFunctionStatements) {


      for ((k, v) <- functionIndexMap) {

        if (s > k && functionIndexMap.get(s).get < v) {
          mapFunctionStatements(k) = mapFunctionStatements.get(k).get - mapFunctionStatements.get(s).get
        }
      }
    }

    NoofStatementsInLargestFunction = mapFunctionStatements.valuesIterator.max
    if(mapFunctionStatements.size > 0)
      AvgNoOfStatementsInFunction = (mapFunctionStatements.valuesIterator.reduceLeft(_+_)) /  (mapFunctionStatements.size)

    Map("NoofStatementsInLargestFunction" -> NoofStatementsInLargestFunction , "AvgNoOfStatementsInFunction" -> AvgNoOfStatementsInFunction)

  }




}
