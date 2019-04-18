/**
  * @author Ankur Gupta
  *         This class is to compute the complexity metrics on javascript projects.
  */



package de.upb.cs.swt.delphi.crawler.Herse



import java.io.File


import org.json4s.JsonAST.{JObject, JValue}
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.JsonMethods._

import sys.process._
import scala.collection.immutable.{ListMap, Range}
import scala.language.dynamics
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.{BigDecimal, log10}



class HerseAnalyzer(jsonAst: String, sourceFile: String, jsonObject: JValue, createObjectMaps: CreateObjectMaps,astComments: String) extends HerseFeatures with Dynamic with AstTraverse {

  implicit val formats = DefaultFormats
  var mapFunctionStatements = scala.collection.mutable.Map[Int, Int]()

  def computeCountComments = {

    (jsonObject \ "comments").toOption match {
      case Some(jArrComments) => val comments = getElement("type", jArrComments)
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


  }

  def computeFileSize = {

    fileSizeinMB = BigDecimal(new File(sourceFile).length().toDouble/(1024*1024).toDouble).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def computeLOC = {

    var count = 0
    val bufferedSource = scala.io.Source.fromFile(sourceFile,"ISO-8859-1")
    val loc: Boolean = true
    var countBlockC = 0
    var countCommentBlankLines = 0
    var startEndMap = scala.collection.mutable.Map[Int, Int]()
    var lineNumber = 0
    var lineFlag = false;
    val ast = astComments

    for ((k, closingIndex) <- createObjectMaps.commentsIndexMap) {

      if (createObjectMaps.commentsMap.get(k).get.contains("Block")) {
        val end = JsonMethods.parse(ast.substring(k, closingIndex + 1)).extract[BlockComment].loc.end.line
        val start = JsonMethods.parse(ast.substring(k, closingIndex + 1)).extract[BlockComment].loc.start.line
        startEndMap += (start -> end)

        countBlockC += (if (end > start) (end - start) + 1 else 1)
      }
    }


    if (startEndMap.nonEmpty) {

      for (line <- bufferedSource.getLines()) {
        lineNumber += 1

        for ((start, end) <- startEndMap) {

          if (!(lineNumber > start && lineNumber < end)) {
            lineFlag = true
          }
          if(end-start == 0 && lineNumber == start) {

            if (line.trim.matches(".+/\\*.*\\*/.*") || line.trim.matches(".+/\\*\\*/.*")
              || line.trim.matches(".*/\\*.*\\*/.+")) {

              countBlockC -= 1
            }
          }
        }
        if ((line.trim.matches("") && lineFlag == true)) {
          count += 1
          lineFlag = false
        } else if((line.trim.matches("//.*") && lineFlag == true)) {
          countBlockC += 1
          lineFlag = false

        }
      }
    } else {

      for (line <- bufferedSource.getLines()) {
        lineNumber += 1



        if ( line.trim.matches("")) {
          count += 1
        } else if(line.trim.matches("//.*")) {
          countBlockC += 1
        }
      }
    }




    bufferedSource.close()



    Sloc = lineNumber - count - countBlockC
    Ploc = lineNumber

  }


  def computeFunctionsCount = {

    val functionsCount = getElement("type", jsonObject)
    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("FunctionDeclaration") match {
      case Some(value) => NoOfFunctionsDeclarations = NoOfFunctionsDeclarations + value
      case None =>
    }



    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("FunctionExpression") match {
      case Some(value) => NoOfFunctionExpressions = NoOfFunctionExpressions + value
      case None =>
    }

    functionsCount.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("ArrowFunctionExpression") match {
      case Some(value) => NoOfArrowFunctionExpressions = NoOfArrowFunctionExpressions + value
      case None =>
    }


  }


  def computeLargestSignature = {

    var signatureSize = scala.collection.mutable.Map[Int,Int]()

    for ((k,v) <- createObjectMaps.functionIndexMap) {

      if(createObjectMaps.functionsMap.get(k).contains("FunctionDeclaration"))  {

        signatureSize += (k -> parse(jsonAst.substring(k,v+1)).extract[FunctionDeclaration].params.size)

      } else if (createObjectMaps.functionsMap.get(k).contains("FunctionExpression")) {

        signatureSize += (k -> parse(jsonAst.substring(k,v+1)).extract[FunctionExpression].params.size)
      } else if (createObjectMaps.functionsMap.get(k).contains("ArrowFunctionExpression")) {

        signatureSize += (k -> parse(jsonAst.substring(k,v+1)).extract[ArrowFunctionExpression].params.size)
      }


    }

    if(signatureSize.size > 0)
      LargestSignatureInFunction = signatureSize.valuesIterator.max



  }


  def computeFunctionStatements = {


    for ((k, v) <- createObjectMaps.functionIndexMap) {
      if(createObjectMaps.functionsMap.get(k).contains("FunctionDeclaration") || createObjectMaps.functionsMap.get(k).contains("FunctionExpression")
        || createObjectMaps.functionsMap.get(k).contains("ArrowFunctionExpression")) {
        getStatements(k, v, jsonAst.substring(k, v+1)) }
    }

    for ((i, l) <- ListMap(functionStatementsMap.toSeq.sortBy(_._1): _*)) {


      var statementSize = l.filter(f => f.contains("Statement") || f.contains("Declaration")).filter(f => !f.contains("Block")).groupBy(identity)
        .mapValues(_.size).foldLeft(0)(_ + _._2)

      mapFunctionStatements = mapFunctionStatements ++ Map(i -> statementSize)


    }
    for ((s, c) <- mapFunctionStatements) {


      for ((k, v) <- createObjectMaps.functionIndexMap) {
        if(createObjectMaps.functionsMap.get(k).contains("FunctionDeclaration") || createObjectMaps.functionsMap.get(k).contains("FunctionExpression")
          || createObjectMaps.functionsMap.get(k).contains("ArrowFunctionExpression")) {
          if (s > k && createObjectMaps.functionIndexMap.get(s).get < v) {
            mapFunctionStatements(k) = mapFunctionStatements.get(k).get - mapFunctionStatements.get(s).get
          } }
      }
    }
    if (mapFunctionStatements.size > 0) {
      NoofStatementsInLargestFunction = mapFunctionStatements.valuesIterator.max

      AvgNoOfStatementsInFunction = (mapFunctionStatements.valuesIterator.reduceLeft(_ + _)) / (mapFunctionStatements.size)
    }



  }

  var log2 = (num: Double) => log10(num) / log10(2.0)

  def computeCC() {

    var mapFunctionsCC = scala.collection.mutable.Map[Int, Int]()


    val pattern = """"type":(.*?),""".r


    for ((k, v) <- createObjectMaps.functionIndexMap) {

      var noOfStatements = 0

      pattern.findAllIn(jsonAst.substring(k, v)).matchData foreach (m => {
        if (m.group(1).contains("Statement") || m.group(1).contains("Expression") || m.group(1).contains("SwitchCase")) {

          m.group(1).replace("\"", "") match {
            case "IfStatement" => noOfStatements = noOfStatements + 1
            case "ForStatement" => noOfStatements = noOfStatements + 1
            case "ForOfStatement" => noOfStatements = noOfStatements + 1
            case "ForInStatement" => noOfStatements = noOfStatements + 1
            case "DoWhileStatement" => noOfStatements = noOfStatements + 1
            case "SwitchCase" => noOfStatements = noOfStatements + 1
            case "WhileStatement" => noOfStatements = noOfStatements + 1
            case "ConditionalExpression" => noOfStatements = noOfStatements + 1
            case _ =>
          }
        }

      })

      "\"SwitchCase\",\"test\":null".r.findAllIn(jsonAst.substring(k, v)).matchData foreach (m => {
        if (m.toString.nonEmpty) noOfStatements = noOfStatements - 1
      })
      """"operator":(.*?),""".r.findAllIn(jsonAst.substring(k, v)).matchData foreach (p => {
        if (p.group(1).contains("||")) noOfStatements = noOfStatements + 1
      })

      noOfStatements = noOfStatements + 1
      mapFunctionsCC += (k -> noOfStatements)
    }
    if (mapFunctionsCC.size > 0) {
      HighestCyclomaticComplexity = mapFunctionsCC.valuesIterator.max
      AvgCyclomaticComplexity = BigDecimal(mapFunctionsCC.valuesIterator.reduceLeft(_ + _).toDouble / mapFunctionsCC.size.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    }


  }

  def computeMI() = {

    val averageLOC = if (NoOfFunctionsDeclarations > 0) Ploc / NoOfFunctionsDeclarations else Ploc

    MaintainabilityIndex = 171 - (3.42 * scala.math.log(if (HalsteadProgramEffort > 0) HalsteadProgramEffort else 1)) - (0.23 * scala.math.log(AvgCyclomaticComplexity)) - (16.2 * scala.math.log(averageLOC))
    if (MaintainabilityIndex > 171) MaintainabilityIndex = 171
    // else MaintainabilityIndex = scala.math.max(0, (MaintainabilityIndex * 100)/171 )
  }

  def computeTotalOperands() = {

    val listOperands = getElement("type", jsonObject)
    listOperands.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("VariableDeclarator") match {
      case Some(value) => TotalNoOfOperands = value
      case None =>
    }

    getVariables("declarations", jsonObject).asInstanceOf[List[Any]].iterator.foreach(f => f match {
      case JObject(obj) =>
      case JArray(arr) =>
      case obj: List[Any] => obj.foreach(o => o match {
        case JObject(v) => if (v.nonEmpty && v.isInstanceOf[List[JField]]) {
          val variableMap = v.toMap
          if (variableMap.get("type").get.values.equals("VariableDeclarator")) {
            listUniqueOperands = (variableMap.get("id").get.values).asInstanceOf[Map[String, String]].get("name").get :: listUniqueOperands
          }
        }
      })
    })

    NoOfUniqueOperands = listUniqueOperands.groupBy(identity).mapValues(_.size).size

  }

  def computeTotalOperators() = {

    val listOperators = getElement("operator", jsonObject)
    TotalNoOfOperators = listOperators.asInstanceOf[List[String]].size
    NoOfUniqueOperators = listOperators.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).size

  }


  def computeHalsteadMetrics() = {

    computeTotalOperators()
    computeTotalOperands()

    HalsteadProgramLength = BigDecimal(TotalNoOfOperators + TotalNoOfOperands).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    if ((NoOfUniqueOperators + NoOfUniqueOperands) > 0) {
      HalsteadProgramVolume = BigDecimal(HalsteadProgramLength * log2(NoOfUniqueOperands + NoOfUniqueOperators)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    if (NoOfUniqueOperands > 0)
      HalsteadDifficulty = BigDecimal((NoOfUniqueOperators * TotalNoOfOperands) / (2 * NoOfUniqueOperands)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    HalsteadProgramEffort = BigDecimal(HalsteadDifficulty * HalsteadProgramVolume).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble


  }


  def complexityMetric: Future[Map[String, Any]] = Future {


    computeCountComments
    computeFileSize
    computeLOC
    computeFunctionsCount
    computeLargestSignature
    computeFunctionStatements
    computeHalsteadMetrics()
    computeCC()
    computeMI()
    Map("fileSizeinMB" -> fileSizeinMB, "SingleLineComments" -> SingleLineComments, "MultiLineComments" -> MultiLineComments,
      "HighestCyclomaticComplexity" -> HighestCyclomaticComplexity, "AvgCyclomaticComplexity" -> AvgCyclomaticComplexity,
      "MaintainabilityIndex" -> MaintainabilityIndex, "Ploc" -> Ploc, "Sloc" -> Sloc,  "NoOfFunctionsDeclarations" -> NoOfFunctionsDeclarations,
      "NoOfFunctionExpressions" -> NoOfFunctionExpressions, "NoOfArrowFunctionExpressions" -> NoOfArrowFunctionExpressions
      , "NoofStatementsInLargestFunction" -> NoofStatementsInLargestFunction, "AvgNoOfStatementsInFunction" -> AvgNoOfStatementsInFunction
      , "LargestSignatureInFunction" -> LargestSignatureInFunction,
      "NoOfUniqueOperands" -> NoOfUniqueOperands, "NoOfUniqueOperators" -> NoOfUniqueOperators, "TotalNoOfOperands" -> TotalNoOfOperands, "TotalNoOfOperators" -> TotalNoOfOperators,
      "HalsteadProgramLength" -> HalsteadProgramLength, "HalsteadProgramVolume" -> HalsteadProgramVolume, "HalsteadDifficulty" -> HalsteadDifficulty,
      "HalsteadProgramEffort" -> HalsteadProgramEffort)
  }


}