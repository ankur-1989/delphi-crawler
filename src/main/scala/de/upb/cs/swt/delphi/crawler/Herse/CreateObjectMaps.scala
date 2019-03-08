package de.upb.cs.swt.delphi.crawler.Herse

import scala.collection.mutable

class CreateObjectMaps(ast: String) {


  var functionsMap = scala.collection.mutable.Map[Int, String]()
  var commentsMap = scala.collection.mutable.Map[Int, String]()
  var objectsMap = scala.collection.mutable.Map[Int, String]()
  var expressionMap = scala.collection.mutable.Map[Int, String]()
  var callExpressionMap = scala.collection.mutable.Map[Int, String]()

  var expressionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var functionIndexMap = scala.collection.mutable.Map[Int,Int]()
  var objectIndexMap = scala.collection.mutable.Map[Int,Int]()
  var commentsIndexMap = scala.collection.mutable.Map[Int,Int]()
  var callExpressionIndexMap = scala.collection.mutable.Map[Int,Int]()

  var index = -1
  var pattern: String = ""

  val objectsList = List("ObjectExpression", "FunctionExpression", "AssignmentExpression", "FunctionDeclaration", "VariableDeclarator", "ObjectPattern",
    "ArrowFunctionExpression", "CallExpression", "Block", "expression")

  def getObjectIndexes(code: String, pattern: String): Unit = {

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

          if(obj.equals("Function")){
            functionIndexMap += (startingIndex -> i)
          } else if( obj.equals("ObjectExpression")) {
            objectIndexMap += (startingIndex -> i)
          } else if(obj.equals("expression")){
            expressionIndexMap += (startingIndex -> i)
          } else if(obj.equals("CallExpression")) {
            callExpressionIndexMap += (startingIndex -> i)
          } else if(obj.equals("TypeStatements")) {

          } else if (obj.equals("Block")) {
            commentsIndexMap += (startingIndex -> i)
          }


          return i
        }
      }
      i= i+1
    }
    return -1
  }

  def createOpeningIndexesMap = {

    objectsList.foreach(f => {

      if (!f.equals("expression"))
        pattern = "{\"type\":" + "\"" + s"$f" + "\""
      else
        pattern = "\"" + s"$f" + "\":{"
      index = -1
      getObjectIndexes(ast, pattern)

    })


  }

  def createClosingIndexMap = {

    for((key,value) <- functionsMap) {
      findClosingIndex(ast,key,"Function")
    }

    for((key,value) <- commentsMap) {
      findClosingIndex(ast,key,"Block")
    }

    for((key,value) <- objectsMap) {
      findClosingIndex(ast,key,"ObjectExpression")
    }

    for((key,value) <- expressionMap) {
      findClosingIndex(ast,key,"expression")
    }
    for((key,value) <- callExpressionMap) {
      findClosingIndex(ast,key,"CallExpression")
    }


  }


}
