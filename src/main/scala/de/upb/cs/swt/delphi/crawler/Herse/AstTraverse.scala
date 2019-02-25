
/**
  * @author Ankur Gupta
  */


package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s
import org.json4s.{JArray, JNothing, JNull, JValue, JsonAST}
import org.json4s.JsonAST.{JField, JObject, JString, JValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait AstTraverse extends HerseFeatures {


  var listUniqueOperands : List[String] = List()
  var functionsMap = scala.collection.mutable.Map[Int, String]()
  var functionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var functionStatementsMap : Map[Int, List[String]] = Map()
  var mapFunctionStatements = scala.collection.mutable.Map[Int, Int]()
  var index = -1

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


  def getObjectIndexes(code: String, pattern : String): Unit = {

    if (code.nonEmpty) {


      index = code.indexOf(pattern, index + 1)
      if (index >=0 && pattern.contains("FunctionExpression")) {
        functionsMap += (index -> "FunctionExpression")

        getObjectIndexes(code, pattern)
      } else if (index>=0 && pattern.contains("FunctionDeclaration")) {
        functionsMap += (index -> "FunctionDeclaration")
        getObjectIndexes(code, pattern)
      } else if(index>=0 && pattern.contains("ArrowFunctionExpression")) {
        functionsMap += (index -> "ArrowFunctionExpression")
        getObjectIndexes(code, pattern)
      } else if (index >0 && pattern.contains("AssignmentExpression")) {
        functionsMap += (index -> "AssignmentExpression")
        getObjectIndexes(code,pattern)
      } else if (index >0 && pattern.contains("VariableDeclarator")) {
        functionsMap += (index -> "VariableDeclarator")
        getObjectIndexes(code,pattern)
      }

    }


  }

  def findClosingIndex(ast: String , startingIndex : Int): Int  = {


    var closingIndex = mutable.Stack[Int]()
    var i = startingIndex

    while (i < ast.length) {


      if(ast.charAt(i).equals('{')) {

        closingIndex.push(ast.charAt(i).asInstanceOf[Int])
      } else if (ast.charAt(i).equals('}')) {
        closingIndex.pop()
        if(closingIndex.isEmpty) {

          functionIndexMap += (startingIndex -> i)
          return i
        }
      }
      i= i+1
    }
    return -1
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
