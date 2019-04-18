package de.upb.cs.swt.delphi.crawler.Herse

import scala.annotation.tailrec
import scala.collection.mutable
import sys.process._

class CreateObjectMaps(ast: String, sourceFile : String, astComments: String) {

  var functionsMap : Map[Int, String] = Map()
  var commentsMap = scala.collection.mutable.Map[Int, String]()
  var objectsMap = scala.collection.mutable.Map[Int, String]()
  var expressionMap : Map[Int, String] = Map()
  var callExpressionMap = scala.collection.mutable.Map[Int, String]()
  var classDeclarationMap = scala.collection.mutable.Map[Int, String]()
  var classDeclarationIndexMap = scala.collection.mutable.Map[Int,Int]()
  var expressionIndexMap : Map[Int,Int] = Map()
  var functionIndexMap : Map[Int,Int] = Map()
  var objectIndexMap = scala.collection.mutable.Map[Int,Int]()
  var commentsIndexMap = scala.collection.mutable.Map[Int,Int]()
  var callExpressionIndexMap = scala.collection.mutable.Map[Int,Int]()
  val loc = true
  var index = -1
  var pattern: String = ""

  val objectsList = List("ObjectExpression", "FunctionExpression", "AssignmentExpression", "FunctionDeclaration", "VariableDeclarator", "ObjectPattern",
    "ArrowFunctionExpression", "CallExpression", "Block", "expression" , "ClassDeclaration")

  def getObjectIndexes(code: String, pattern: String): Unit = {

    @tailrec
    def getObjectIndexesInside(code: String, pattern: String) : Unit = {

      if (code.nonEmpty) {


        index = code.indexOf(pattern, index + 1)
        if (index >= 0 && pattern.contains("FunctionExpression")) {
          functionsMap += (index -> "FunctionExpression")

          getObjectIndexesInside(code, pattern)
        } else if (index >= 0 && pattern.contains("FunctionDeclaration")) {
          functionsMap += (index -> "FunctionDeclaration")
          getObjectIndexesInside(code, pattern)
        } else if (index >= 0 && pattern.contains("ArrowFunctionExpression")) {
          functionsMap += (index -> "ArrowFunctionExpression")
          getObjectIndexesInside(code, pattern)
        }  else if (index > 0 && pattern.contains("VariableDeclarator")) {
          functionsMap += (index -> "VariableDeclarator")
          getObjectIndexesInside(code, pattern)
        } else if (index > 0 && pattern.contains("Block")) {
          commentsMap += (index -> "Block")
          getObjectIndexesInside(code, pattern)
        } else if (index > 0 && pattern.contains("ObjectExpression")) {
          objectsMap += (index -> "ObjectExpression")
          getObjectIndexesInside(code, pattern)
        } else if (index > 0 && pattern.contains("ObjectPattern")) {
          objectsMap += (index -> "ObjectPattern")
          getObjectIndexesInside(code, pattern)
        }
        else if (index > 0 && pattern.contains("CallExpression")) {
          callExpressionMap += (index -> "expression")
          getObjectIndexesInside(code, pattern)
        } else if (index > 0 && pattern.contains("ClassDeclaration")) {
          classDeclarationMap += (index -> "ClassDeclaration")
          getObjectIndexesInside(code, pattern)
        }


      }

    }

    getObjectIndexesInside(code,pattern)

  }


  def getExpressionIndexes (code : String, pattern: String) = {

    @tailrec
    def getExpressionIndexesInside (code : String, pattern: String): Unit = {

      if(code.nonEmpty) {
        index = code.indexOf(pattern , index+1)
        if(index > 0 &&  pattern.contains("expression")) {
          expressionMap += (index -> "expression")
          getExpressionIndexesInside(code,pattern)
        }
      }

    }
    getExpressionIndexesInside(code,pattern)
  }

  def getAssignmentExpressionIndexes (code : String, pattern: String) = {

    @tailrec
    def getAssignmentExpressionIndexesInside (code : String, pattern: String): Unit = {

      if(code.nonEmpty) {
        index = code.indexOf(pattern , index+1)
        if(index > 0 &&  pattern.contains("AssignmentExpression")) {
          functionsMap += (index -> "AssignmentExpression")
          getAssignmentExpressionIndexesInside(code,pattern)
        }
      }

    }
    getAssignmentExpressionIndexesInside(code,pattern)
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
            expressionIndexMap = expressionIndexMap ++ Map(startingIndex -> i)
          } else if(obj.equals("CallExpression")) {
            callExpressionIndexMap += (startingIndex -> i)
          } else if(obj.equals("TypeStatements")) {

          } else if (obj.equals("Block")) {
            commentsIndexMap += (startingIndex -> i)
          } else if (obj.equals("Line")) {
            commentsIndexMap += (startingIndex -> i)
          } else if (obj.equals("ClassDeclaration")) {
            classDeclarationIndexMap += (startingIndex -> i)
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

      if(f.equals("CallExpression")) {

        index = -1
        for((k,v) <- expressionIndexMap) {

          getObjectIndexes(ast.substring(k,v+1), pattern)

        }

      } else if (f.equals("AssignmentExpression")){
        index = -1
        getAssignmentExpressionIndexes(ast,pattern)

      } else if(f.equals("expression")) {
        index = -1
        getExpressionIndexes(ast,pattern)
      } else if (f.equals("Block") || f.equals("Line")) {

        index  = -1
        getObjectIndexes(astComments,pattern)
      }
      else {
        index = -1
        getObjectIndexes(ast, pattern)
      }





    })


  }

  def createClosingIndexMap = {


    for((key,value) <- functionsMap) {
      findClosingIndex(ast,key,"Function")
    }

    for((key,value) <- commentsMap) {
      if(value.contains("Block"))
        findClosingIndex(astComments,key,"Block")
    }

    for((key,value) <- objectsMap) {
      findClosingIndex(ast,key,"ObjectExpression")
    }

    for((key,value) <- classDeclarationMap) {
      findClosingIndex(ast,key,"ClassDeclaration")
    }

    for((key,value) <- expressionMap) {
      findClosingIndex(ast,key,"expression")
    }
    for((key,value) <- expressionIndexMap) {
      for((i,j) <- callExpressionMap) {
        findClosingIndex(ast.substring(key,value+1),i,"CallExpression")
      }
    }


  }


}