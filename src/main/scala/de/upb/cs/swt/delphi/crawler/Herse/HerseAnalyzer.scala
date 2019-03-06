/**
  * @author Ankur Gupta
  *         This class is to compute the complexity metrics on javascript projects.
  */



package de.upb.cs.swt.delphi.crawler.Herse



import org.json4s.JsonAST.{JObject, JValue}
import org.json4s._
import org.json4s.jackson.JsonMethods
import sys.process._
import scala.collection.immutable.ListMap
import scala.language.dynamics
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.{BigDecimal, log10}




class HerseAnalyzer(jsonAst: String, sourceFile: String, jsonObject: JValue) extends HerseFeatures with Dynamic with AstTraverse {

  implicit val formats = DefaultFormats

  def computeCountComments = {

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


  }

  def computeLOC = {

    var count = 0
    val bufferedSource = scala.io.Source.fromFile(sourceFile)
    val loc: Boolean = true
    var countBlockC = 0
    val ast = (s"node src/main/resources/parser.js ${sourceFile} ${loc}".!!).toString
    getObjectIndexes(ast,"{\"type\":\"Block\"")
    for ( (k,v) <- commentsMap) {
      if(v.contains("Block")) {
        val closingIndex = findClosingIndex(ast, k,"Block")
        val end = JsonMethods.parse(ast.substring(k, closingIndex + 1)).extract[BlockComment].loc.end.line
        val start  = JsonMethods.parse(ast.substring(k, closingIndex + 1)).extract[BlockComment].loc.start.line
        if (end > start) countBlockC += (end-start)+1 else 1
      }
    }


    for(line <- bufferedSource.getLines()) {
      Ploc += 1
      if(line.trim.matches("") || line.trim.matches("//.*"))  {
        count+= 1

      }
    }
    bufferedSource.close()

    Ploc = Ploc - count - countBlockC

  }



  def computeFunctionsCount = {

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



  }


  def computeLargestSignature(node: Any) = {


    checkParams(node.asInstanceOf[JValue])



  }


  def computeFunctionStatements = {


    getObjectIndexes(jsonAst,"{\"type\":\"FunctionDeclaration\"")
    getObjectIndexes(jsonAst,"{\"type\":\"FunctionExpression\"")
    getObjectIndexes(jsonAst,"{\"type\":\"ArrowFunctionExpression\"")

    for((k,v) <- functionsMap) {
      findClosingIndex(jsonAst,k,"Function")
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
    if(mapFunctionStatements.size > 0) {
      NoofStatementsInLargestFunction = mapFunctionStatements.valuesIterator.max

      AvgNoOfStatementsInFunction = (mapFunctionStatements.valuesIterator.reduceLeft(_ + _)) / (mapFunctionStatements.size)
    }


  }

  var log2 = (num: Double) => log10(num)/log10(2.0)

  def computeCC() {

    var mapFunctionsCC = scala.collection.mutable.Map[Int,Int]()
    getObjectIndexes(jsonAst,"{\"type\":\"FunctionDeclaration\"")
    getObjectIndexes(jsonAst,"{\"type\":\"FunctionExpression\"")
    getObjectIndexes(jsonAst,"{\"type\":\"ArrowFunctionExpression\"")

    for((k,v) <- functionsMap) {
      findClosingIndex(jsonAst,k,"Function")
    }

    val pattern = """"type":(.*?),""".r


    for((k,v) <- functionIndexMap) {

      var noOfStatements = 0

      pattern.findAllIn(jsonAst.substring(k,v)).matchData foreach(m =>
      {
        if(m.group(1).contains("Statement") || m.group(1).contains("Expression") || m.group(1).contains("SwitchCase")) {

          m.group(1).replace("\"","") match {
            case "IfStatement" =>  noOfStatements = noOfStatements + 1
            case "ForStatement" => noOfStatements = noOfStatements + 1
            case "ForOfStatement" => noOfStatements = noOfStatements + 1
            case "ForInStatement" => noOfStatements = noOfStatements + 1
            case "DoWhileStatement" => noOfStatements = noOfStatements + 1
            case "SwitchCase" =>   noOfStatements = noOfStatements+1
            case "WhileStatement" => noOfStatements = noOfStatements + 1
            case "ConditionalExpression" => noOfStatements = noOfStatements + 1
            case _ =>
          }
        }

      })

      "\"SwitchCase\",\"test\":null".r.findAllIn(jsonAst.substring(k,v)).matchData foreach(m => {
        if(m.toString.nonEmpty) noOfStatements = noOfStatements-1
      })
      """"operator":(.*?),""".r.findAllIn(jsonAst.substring(k,v)).matchData foreach(p => {
        if(p.group(1).contains("||")) noOfStatements = noOfStatements+1 })

      noOfStatements = noOfStatements + 1
      mapFunctionsCC += (k -> noOfStatements)
    }
    if(mapFunctionsCC.size > 0) {
      HighestCyclomaticComplexity = mapFunctionsCC.valuesIterator.max
      AvgCyclomaticComplexity = mapFunctionsCC.valuesIterator.reduceLeft(_ + _) / mapFunctionsCC.size
    }

  }

  def computeMI() = {

    val averageLOC = if(NoOfFunctionsDeclarations > 0) Ploc/NoOfFunctionsDeclarations else Ploc

    MaintainabilityIndex = 171 - (3.42 * scala.math.log(if(HalsteadProgramEffort>0)  HalsteadProgramEffort else 1)) - (0.23 * scala.math.log(AvgCyclomaticComplexity)) - (16.2 * scala.math.log(averageLOC))
    if (MaintainabilityIndex > 171) MaintainabilityIndex =171
    // else MaintainabilityIndex = scala.math.max(0, (MaintainabilityIndex * 100)/171 )
  }

  def computeTotalOperands()  = {

    val listOperands = getElement("type", jsonObject)
    listOperands.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("VariableDeclarator") match {
      case Some(value) => TotalNoOfOperands = value
      case None =>
    }

    getVariables("declarations",jsonObject).asInstanceOf[List[Any]].iterator.foreach( f => f match {
      case JObject(obj) => println(obj)
      case JArray(arr) => println(arr)
      case obj: List[Any] => obj.foreach(o => o match {
        case JObject(v) => if(v.nonEmpty && v.isInstanceOf[List[JField]]) {
          val variableMap = v.toMap
          if(variableMap.get("type").get.values.equals("VariableDeclarator")){
            listUniqueOperands =  (variableMap.get("id").get.values).asInstanceOf[Map[String,String]].get("name").get :: listUniqueOperands
          }
        }
      })
    })

    NoOfUniqueOperands = listUniqueOperands.groupBy(identity).mapValues(_.size).size

  }

  def computeTotalOperators()  = {

    val listOperators = getElement("operator", jsonObject)
    TotalNoOfOperators = listOperators.asInstanceOf[List[String]].size
    NoOfUniqueOperators = listOperators.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).size

  }



  def computeHalsteadMetrics() = {

    computeTotalOperators()
    computeTotalOperands()

    HalsteadProgramLength =  BigDecimal(TotalNoOfOperators + TotalNoOfOperands).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    if ( (NoOfUniqueOperators + NoOfUniqueOperands) > 0 ) {
      HalsteadProgramVolume = BigDecimal(HalsteadProgramLength * log2(NoOfUniqueOperands + NoOfUniqueOperators)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    if(NoOfUniqueOperands > 0)
      HalsteadDifficulty = BigDecimal((NoOfUniqueOperators * TotalNoOfOperands)/ (2*NoOfUniqueOperands)).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble

    HalsteadProgramEffort =  BigDecimal( HalsteadDifficulty * HalsteadProgramVolume).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble



  }



  def complexityMetric : Future[Map[String,Any]] = Future {


    computeCountComments
    computeLOC
    computeFunctionsCount
    computeLargestSignature(jsonObject)
    computeFunctionStatements
    computeHalsteadMetrics()
    computeCC()
    computeMI()
    Map("SingleLineComments" -> SingleLineComments, "MultiLineComments" -> MultiLineComments,
      "HighestCyclomaticComplexity" -> HighestCyclomaticComplexity, "AvgCyclomaticComplexity" -> AvgCyclomaticComplexity,
      "MaintainabilityIndex" -> MaintainabilityIndex , "Sloc" -> Ploc , "NoOfFunctionsDeclarations" -> NoOfFunctionsDeclarations
      ,"NoofStatementsInLargestFunction" -> NoofStatementsInLargestFunction , "AvgNoOfStatementsInFunction" -> AvgNoOfStatementsInFunction
      ,"LargestSignatureInFunction" -> LargestSignatureInFunction,
      "NoOfUniqueOperands"-> NoOfUniqueOperands, "NoOfUniqueOperators" -> NoOfUniqueOperators, "TotalNoOfOperands" -> TotalNoOfOperands,  "TotalNoOfOperators" -> TotalNoOfOperators,
      "HalsteadProgramLength" -> HalsteadProgramLength , "HalsteadProgramVolume" -> HalsteadProgramVolume , "HalsteadDifficulty" -> HalsteadDifficulty,
      "HalsteadProgramEffort" -> HalsteadProgramEffort)
  }



}
