
/**
  * @author Ankur Gupta
  */


package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s
import org.json4s.{JArray, JNothing, JNull, JValue, JsonAST}
import org.json4s.JsonAST._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait AstTraverse extends HerseFeatures {


  var listUniqueOperands: List[String] = List()
  var listStatement: List[String] = List()
  var r = scala.util.Random
  var functionsMap = scala.collection.mutable.Map[Int, String]()
  var commentsMap = scala.collection.mutable.Map[Int, String]()
  var objectsMap = scala.collection.mutable.Map[Int, String]()
  var expressionMap = scala.collection.mutable.Map[Int,String]()
  var expressionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var functionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var objectIndexMap = scala.collection.mutable.Map[Int,Int]()
  var commentsIndexMap = scala.collection.mutable.Map[Int,Int]()
  var callExpressionMap = scala.collection.mutable.Map[Int,String]()
  var callExpressionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var functionStatementsMap : Map[Int, List[String]] = Map()
  var mapFunctionStatements = scala.collection.mutable.Map[Int, Int]()
  var objectPropertiesMap = scala.collection.mutable.Map[Int, Int]()
  var callingChainMap =  scala.collection.mutable.Map[Int,Int]()
  var mapFunctionCallees = scala.collection.mutable.Map[String,List[String]]()
  var typeStatementsMap = scala.collection.mutable.Map[Int,String]()
  var typeStatementsIndexMap =  scala.collection.mutable.Map[Int,Int]()
  var mapStatementsNesting = scala.collection.mutable.Map[Int, Int]()
  var index = -1
  var lines : Array[String] = Array()

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
      case JDouble(num) =>
      case JsonAST.JLong(num) =>
      case JsonAST.JSet(set) =>
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

  def getObjectIndexes(code: String, pattern : String): Unit = {

    if (code.nonEmpty) {


      index = code.indexOf(pattern, index + 1)
      if (index >= 0 && pattern.contains("FunctionExpression")) {
        functionsMap += (index -> "FunctionExpression")

        getObjectIndexes(code, pattern)
      } else if (index >= 0 && pattern.contains("FunctionDeclaration")) {
        functionsMap += (index -> "FunctionDeclaration")
        getObjectIndexes(code, pattern)
      } else if (index >= 0 && pattern.contains("ArrowFunctionExpression")) {
        functionsMap += (index -> "ArrowFunctionExpression")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("AssignmentExpression")) {
        functionsMap += (index -> "AssignmentExpression")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("VariableDeclarator")) {
        functionsMap += (index -> "VariableDeclarator")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("Block")) {
        commentsMap += (index -> "Block")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("ObjectExpression")) {
        objectsMap += (index -> "ObjectExpression")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("ObjectPattern")) {
        objectsMap += (index -> "ObjectPattern")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("expression")) {
        expressionMap += (index -> "expression")
        getObjectIndexes(code, pattern)
      } else if (index > 0 && pattern.contains("CallExpression")) {
        callExpressionMap += (index -> "expression")
        getObjectIndexes(code, pattern)
      }


    }
  }

  def findClosingIndex(ast: String , startingIndex : Int, obj: String): Int  = {


    var closingIndex = mutable.Stack[Int]()
    var i = startingIndex

    while (i < ast.length) {


      if(ast.charAt(i).equals('{')) {

        closingIndex.push(ast.charAt(i).asInstanceOf[Int])
      } else if (ast.charAt(i).equals('}')) {
        closingIndex.pop()
        if(closingIndex.isEmpty) {

          if(!obj.equals("Block") || obj.equals("FunctionExpression") || obj.equals("FunctionDeclaration")
            || obj.equals("ArrowFunctionExpression") || obj.equals("AssignmentExpression") || obj.equals("VariableDeclarator") ){
            functionIndexMap += (startingIndex -> i)
          } else if( obj.equals("ObjectExpression") || obj.equals("ObjectPattern")) {
            objectIndexMap += (startingIndex -> i)
          } else if(obj.equals("expression")){
            expressionIndexMap += (startingIndex -> i)
          } else if(obj.equals("CallExpression")) {
            callExpressionIndexMap += (startingIndex -> i)
          } else if(obj.equals("TypeStatements")) {}

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
