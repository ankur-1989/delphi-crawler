package de.upb.cs.swt.delphi.crawler.Herse

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ObjectFeatures(jsonAst: String,createObjectMaps: CreateObjectMaps) extends AstTraverse {

  var objectPropertiesMap = scala.collection.mutable.Map[Int, Int]()
  var callingChainMap =  scala.collection.mutable.Map[Int,Int]()

  def computeObjectsCount = {


    NoOfObjects = createObjectMaps.objectsMap.size

    var pattern = """"type":(.*?),""".r
    for ((k,v) <- createObjectMaps.objectIndexMap) {

      var propertiesCount = 0
      pattern.findAllIn(jsonAst.substring(k,v+1)).matchData foreach(m =>
      {
        if(m.group(1).contains("Property")) {
          propertiesCount += 1
        }
      })

      objectPropertiesMap =  objectPropertiesMap ++ Map(k -> propertiesCount)


    }

    for( (i,j) <- objectPropertiesMap ) {

      for((k,v) <- createObjectMaps.objectIndexMap) {

        if((i > k) && createObjectMaps.objectIndexMap.get(i).get < v) {
          objectPropertiesMap(k) = objectPropertiesMap.get(k).get - objectPropertiesMap.get(i).get
        }

      }

    }
    if(objectPropertiesMap.size > 0) {
      NoOfPropertiesInLargestObject =  objectPropertiesMap.valuesIterator.max
      AvgNoOfPropertiesPerObject = (objectPropertiesMap.valuesIterator.reduceLeft(_+_)) /  (objectPropertiesMap.size)
    }


  }


  def computeLLOC = {

    val pattern = """"type":(.*?),""".r
    pattern.findAllIn(jsonAst).matchData foreach( m => {

      m.group(1).replace("\"","") match {
        case  "FunctionDeclaration" => Lloc+= 1
        case "ExpressionStatement" => Lloc+= 1
        case "VariableDeclarator" => Lloc+= 1
        case "ForStatement" => Lloc += 1
        case "IfStatement" => Lloc+= 1
        case "ForOfStatement" => Lloc+= 1
        case "ForInStatement" => Lloc+= 1
        case "DoWhileStatement" => Lloc+= 1
        case "WhileStatement" => Lloc+= 1
        case "TryStatement" => Lloc+= 1
        case "CatchClause" => Lloc+= 1
        case "SwitchStatement" => Lloc+= 1
        case "BreakStatement" => Lloc+= 1
        case "ContinueStatement" => Lloc+= 1
        case "ConditionalExpression" =>  Lloc+= 1
        case "ReturnStatement" => Lloc+= 1

        case _ =>

      }
    })

    """"alternate":(.*?),""".r.findAllIn(jsonAst).matchData foreach(m => {
      if(!m.group(1).contains("null")) {
        Lloc+= 1
      }

    })
    if(objectPropertiesMap.size > 0) {
      for((k,v) <- objectPropertiesMap) {
        if(createObjectMaps.objectsMap.get(k).get.contains("ObjectExpression"))
          Lloc+= v
      }
    }

  }

  def computeMessageChainCount: Unit = {

    for ((eS,eC) <- createObjectMaps.expressionIndexMap) {
      var messageChain = 0
      for ((ceS,ceC) <- createObjectMaps.callExpressionIndexMap) {

        if(ceS > eS && ceC < eC) {
          messageChain+= 1
        }

      }
      callingChainMap = callingChainMap ++ Map(eS -> messageChain)
    }

    if(callingChainMap.size > 0 ) {
      LongestCallingChain = callingChainMap.valuesIterator.max
      AvgCallingChain = callingChainMap.valuesIterator.reduceLeft(_+_) / callingChainMap.size
    }

  }

  def computeObjectMetrics : Future[Map[String,Any]] = Future {

    computeObjectsCount
    computeLLOC
    computeMessageChainCount

    Map("Lloc" -> Lloc, "NoOfObjects" -> NoOfObjects , "NoOfPropertiesInLargestObject" -> NoOfPropertiesInLargestObject ,
      "AvgNoOfPropertiesPerObject" -> AvgNoOfPropertiesPerObject , "LongestCallingChain" -> LongestCallingChain, "AvgCallingChain" -> AvgCallingChain )

  }


}
