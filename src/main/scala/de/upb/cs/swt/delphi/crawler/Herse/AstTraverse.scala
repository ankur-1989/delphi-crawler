
/**
  * @author Ankur Gupta
  */


package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s
import org.json4s.{JArray, JNothing, JNull, JValue, JsonAST}
import org.json4s.JsonAST.{JValue, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait AstTraverse extends HerseFeatures {

  var listUniqueOperands: List[String] = List()
  var listStatement: List[String] = List()
  var r = scala.util.Random
  var functionStatementsMap : Map[Int, List[String]] = Map()
  var typeStatementsMap = scala.collection.mutable.Map[Int,String]()
  var typeStatementsIndexMap =  scala.collection.mutable.Map[Int,Int]()
  var mapStatementsNesting = scala.collection.mutable.Map[Int, Int]()
  var index = -1
  var lines : Array[String] = Array()


  def checkParams(node: JValue): Any = {

    node match {
      case JString(s) => s
      case JNull =>
      case JNothing =>
      case JObject(obj) => if (obj.nonEmpty) obj.foreach(f => if (f._1.contains("params")) LargestSignatureInFunction = if (f._2.children.size > LargestSignatureInFunction) f._2.children.size else LargestSignatureInFunction
      else checkParams(f._2))
      case JsonAST.JArray(arr) => if (arr.nonEmpty) arr.foreach(f => checkParams(f))
      case JsonAST.JBool(value) =>
      case JsonAST.JDecimal(num) =>
      case JsonAST.JInt(num) =>
      case JDouble(num) =>
      case JsonAST.JLong(num) =>
      case JsonAST.JSet(set) =>
    }

  }


  def getElement(elem: String, json: JValue) = for {
    JObject(child) <- json
    JField(`elem`, JString(value)) <- child
  } yield value


  def getVariables(elem: String, json: JValue) = for {
    JObject(child) <- json
    JField(`elem`, JArray(arr)) <- child
  } yield arr

  def getTypeIndexes(code: String, pattern: String, stmtType: String) : Unit = {
    if(code.nonEmpty) {
      index = code.indexOf(pattern, index+1)
      if(index >=0 ) {
        typeStatementsMap += (index -> stmtType)
        getTypeIndexes(code,pattern,stmtType)
      }

    }

  }

  def findTypeClosingIndex(ast: String, startingIndex : Int): Unit = {

    var closingIndex = mutable.Stack[Int]()
    var i = startingIndex

    while (i < ast.length) {


      if(ast.charAt(i).equals('{')) {

        closingIndex.push(ast.charAt(i).asInstanceOf[Int])
      } else if (ast.charAt(i).equals('}')) {
        closingIndex.pop()
        if(closingIndex.isEmpty) {

          typeStatementsIndexMap += (startingIndex -> i)
          return
        }
      }
      i= i+1
    }

  }







  def getStatements(startIndex: Int, endIndex: Int, ast: String) = {
    val pattern = """"type":(.*?),""".r

    var statementList = new  ListBuffer[String]()

    pattern.findAllIn(ast).matchData foreach( m =>
    {
      if(!m.group(1).contains("BlockStatement") && !m.group(1).contains("FunctionDeclaration")) {
        statementList += m.group(1).toString
      }})

    functionStatementsMap = functionStatementsMap ++ Map( startIndex -> statementList.toList)

  }





}
