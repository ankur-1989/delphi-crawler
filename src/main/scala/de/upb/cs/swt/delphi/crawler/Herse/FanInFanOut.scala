package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import scala.concurrent.Future
import org.json4s.jackson.JsonMethods
import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext.Implicits.global

class FanInFanout(ast: String) extends HerseFeatures with AstTraverse {

  implicit val format = DefaultFormats


  var functionName: String = ""
  var totalFanOut = 0
  var fanIn = 0
  var totalFanIn = 0
  var ccCount = 0
  var mapfunctionCC = scala.collection.mutable.Map[Int,Int]()
  var mapFunctionIsRecursion = scala.collection.mutable.Map[Int,Int]()
  var mapIfOperators = scala.collection.mutable.Map[Int,List[String]]()
  var firstOperator: String = ""
  var secondOperator: String = ""
  def computeFanInFanOut = {

    getObjectIndexes(ast, "{\"type\":\"FunctionDeclaration\"")
    getObjectIndexes(ast, "{\"type\":\"AssignmentExpression\"")
    getObjectIndexes(ast, "{\"type\":\"VariableDeclarator\"")
    getObjectIndexes(ast, "{\"type\":\"ArrowFunctionExpression\"")
    getObjectIndexes(ast, "{\"type\":\"FunctionExpression\"")



    for ((k, v) <- ListMap(functionsMap.toSeq.sortBy(_._1):_*)) {


      var functionList: List[String] = List()
      val closingIndex = findClosingIndex(ast, k, "Function")
      val obj = parse(ast.substring(k, closingIndex + 1))
      if (closingIndex > 0 && v.contains("AssignmentExpression")) {

        (obj \ "right" \ "type").toOption match {
          case Some(value) =>

            if (value.values.toString.contains("FunctionExpression") || value.values.toString.contains("ArrowFunctionExpression")) {
              (obj \ "left" \ "type").toOption match {
                case Some(v) => if (v.values.toString.equals("MemberExpression")) {

                  obj.extract[AssignmentMemberExpression].left.`object`.keySet.foreach(f =>
                    if (f.equals("name")) {
                      functionName = obj.extract[AssignmentMemberExpression].left.`object`.get("name").get.toString.concat(".").concat(obj.extract[AssignmentMemberExpression].left.property.name)
                    } else if (f.equals("object")) {
                      functionName = obj.extract[AssignmentMemberExpression].left.`object`.get("object").get.asInstanceOf[Map[String, Any]].get("name").get.toString.concat(".")
                        .concat(obj.extract[AssignmentMemberExpression].left.`object`.get("property").get.asInstanceOf[Map[String, Any]].get("name").get.toString).concat(".")
                        .concat(obj.extract[AssignmentMemberExpression].left.property.name)
                    }

                  )


                } else if (v.values.toString.equals("Identifier")) {
                  functionName = obj.extract[AssignmentIdentifierExpression].left.name
                }
                case None =>
              }


              (obj \\ "callee").toOption match {
                case Some(callee) => if ("callee".r.findAllIn(compact(render(obj))).matchData.toList.size > 1) {
                  callee.children.foreach(f =>
                    (f \ "type").toOption match {
                      case Some(t) => if (t.values.toString.contains("MemberExpression"))
                        functionList = f.extract[CalleeMemberExpression].`object` match {
                          case Some(n) => n.name + "." + f.extract[CalleeMemberExpression].property.name :: functionList
                          case None => f.extract[CalleeMemberExpression].property.name :: functionList
                        }
                      else if (t.values.toString.contains("Identifier"))
                        functionList = f.extract[CalleeIdentifierExpression].name :: functionList
                      case None =>
                    }

                  ) }
                else {
                  callee.children.foreach(f =>
                    (f \ "type").toOption match {
                      case Some(t) => if (t.values.toString.contains("MemberExpression"))
                        functionList = f.extract[CalleeMemberExpression].`object` match {
                          case Some(n) => n.name + "." + f.extract[CalleeMemberExpression].property.name :: functionList
                          case None => f.extract[CalleeMemberExpression].property.name :: functionList
                        }
                      else if (t.values.toString.contains("Identifier"))
                        functionList = f.extract[CalleeIdentifierExpression].name :: functionList
                      case None =>
                    }

                  )
                }
                case None =>
              }


            }
          case None =>
        }
        mapFunctionCallees = mapFunctionCallees ++ Map(functionName -> functionList.distinct)

      }
      else if (closingIndex > 0 && v.contains("FunctionDeclaration")) {

        functionName = obj.extract[FunctionDeclaration].id.name




        (obj \\ "callee").toOption match {
          case Some(callee) =>
            if ("callee".r.findAllIn(compact(render(obj))).matchData.toList.size > 1) {

              callee.children.foreach(f => {

                (f \ "type").toOption match {
                  case Some(t) =>
                    if (t.values.toString.contains("MemberExpression")) {
                      functionList = f.extract[CalleeMemberExpression].`object` match {
                        case Some(n) => n.name + "." + f.extract[CalleeMemberExpression].property.name :: functionList
                        case None => f.extract[CalleeMemberExpression].property.name :: functionList
                      }
                    }
                    else if (t.values.toString.contains("Identifier")) {


                      functionList = f.extract[CalleeIdentifierExpression].name :: functionList


                    }
                  case None =>
                }
              }
              )
            } else {
              (callee \ "type").toOption match {
                case Some(t) =>
                  if (t.values.toString.contains("MemberExpression")) {
                    functionList = callee.extract[CalleeMemberExpression].`object` match {
                      case Some(n) => n.name + "." + callee.extract[CalleeMemberExpression].property.name :: functionList
                      case None => callee.extract[CalleeMemberExpression].property.name :: functionList
                    }
                  }
                  else if (t.values.toString.contains("Identifier")) {

                    functionList = callee.extract[CalleeIdentifierExpression].name :: functionList


                  }
                case None =>

              }
            }

          case None =>
        }

        mapFunctionCallees = mapFunctionCallees ++ Map(functionName -> functionList.distinct)

      }
      else if (closingIndex > 0 && v.contains("VariableDeclarator")) {


        (obj \ "init" \ "type").toOption match {
          case Some(fe) => if (fe.values.toString.contains("FunctionExpression") || fe.values.toString.contains("ArrowFunctionExpression")) {
            functionName = obj.extract[VariableDeclarator].id.name

            (obj \\ "callee").toOption match {

              case Some(callee) =>  if ("callee".r.findAllIn(compact(render(obj))).matchData.toList.size > 1) {

                callee.children.foreach(f => {

                  (f \ "type").toOption match {
                    case Some(t) =>
                      if (t.values.toString.contains("MemberExpression")) {
                        functionList = f.extract[CalleeMemberExpression].`object` match {
                          case Some(n) => n.name + "." + f.extract[CalleeMemberExpression].property.name :: functionList
                          case None => f.extract[CalleeMemberExpression].property.name :: functionList
                        }
                      }
                      else if (t.values.toString.contains("Identifier")) {


                        functionList = f.extract[CalleeIdentifierExpression].name :: functionList


                      }
                    case None =>
                  }
                }
                )
              } else {
                (callee \ "type").toOption match {
                  case Some(t) =>
                    if (t.values.toString.contains("MemberExpression")) {
                      functionList = callee.extract[CalleeMemberExpression].`object` match {
                        case Some(n) => n.name + "." + callee.extract[CalleeMemberExpression].property.name :: functionList
                        case None => callee.extract[CalleeMemberExpression].property.name :: functionList
                      }
                    }
                    else if (t.values.toString.contains("Identifier")) {

                      functionList = callee.extract[CalleeIdentifierExpression].name :: functionList


                    }
                  case None =>

                }
              }
              case None =>

            }


          }
          case None =>
        }


        mapFunctionCallees = mapFunctionCallees ++ Map(functionName -> functionList.distinct)

      }


      if(functionList.distinct.contains(functionName)) {


        if((v.contains("AssignmentExpression") || v.contains("VariableDeclarator"))) {
          for( (key,value) <- ListMap(functionsMap.toSeq.sortBy(_._1):_*)) {
            if(key > k && (value.contains("FunctionExpression") || value.contains("ArrowFunctionExpression")) &&
              (v.contains("VariableDeclarator") || v.contains("AssignmentExpression"))){

              mapFunctionIsRecursion += ( key -> 1)  }
          }
        } else if(v.contains("FunctionDeclaration")) {
          mapFunctionIsRecursion += (k -> 1)
        }
      }


    }


    for ((k, v) <- mapFunctionCallees) {

      if (v.size > HighestFanOut)
        HighestFanOut = v.size

      totalFanOut = totalFanOut + v.size

      for ((fname, callees) <- mapFunctionCallees) {

        if (callees.contains(k)) {
          fanIn = fanIn + 1
        }

      }

      if (fanIn > HighestFanIn) {
        HighestFanIn = fanIn
      }

      totalFanIn = totalFanIn + fanIn

    }

    if (mapFunctionCallees.size > 0) {
      AvgFanOut = totalFanOut / mapFunctionCallees.size
      AvgFanIn = totalFanIn / mapFunctionCallees.size
    }


  }

  def computeCognitiveComplexity = {


    for((k,v) <- functionIndexMap) {
      ccCount = 0
      typeStatementsMap.clear()
      typeStatementsIndexMap.clear()
      mapStatementsNesting.clear()
      mapIfOperators.clear()
      var calleeList : List[String] = List()


      if( functionsMap.get(k).get.equals("FunctionDeclaration") || functionsMap.get(k).get.equals("FunctionExpression") || functionsMap.get(k).get.equals("ArrowFunctionExpression")) {

        val body = JsonMethods.parse(ast.substring(k, v + 1))
        val statements = getElement("type", body)

        statements.asInstanceOf[List[String]].filter(f => f.equals("IfStatement") || f.equals("ForStatement") || f.equals("ForInStatement") || f.equals("ForOfStatement")
          || f.equals("SwitchStatement") || f.equals("WhileStatement") || f.equals("DoWhileStatement") || f.equals("CatchClause") || f.equals("FunctionExpression")
          || f.equals("FunctionDeclaration") || f.equals("ArrowFunctionExpression") || f.equals("ConditionalExpression")).distinct.
          foreach(m => {
            val pattern: String = "{\"type\":" + "\"" + s"$m" + "\""

            getTypeIndexes(ast.substring(k, v + 1), pattern, m)
          })
        typeStatementsMap.remove(0)
        for ((i, j) <- typeStatementsMap) {

          findTypeClosingIndex(ast.substring(k, v + 1), i)
        }
        for ((tk, tv) <- typeStatementsMap) {
          var nesting = 0
          for ((ti, tc) <- typeStatementsIndexMap) {
            if (tk > ti && typeStatementsIndexMap.get(tk).get < tc && tk != ti) {
              nesting += 1
            }
          }
          mapStatementsNesting += (tk -> nesting)
        }

        statements.asInstanceOf[List[String]].filter( f => f.equals("BreakStatement") || f.equals("ContinueStatement")).foreach( m =>
          ccCount += 1
        )

        for((key,value) <- mapStatementsNesting) {

          typeStatementsMap.get(key).get match {
            case "IfStatement" =>     var operatorList : List[String] = List()
              """"operator":(.*?),""".r.findAllIn(ast.substring(key,typeStatementsIndexMap.get(key).get)).matchData foreach (f => {
                val operator = f.group(1).replace("\"","")
                if(operator.equals("||") || operator.equals("&&") || operator.equals("!")) {
                  operatorList = operator :: operatorList
                }

              })
              mapIfOperators += (key -> operatorList)
              ccCount += value+1
            case "ForStatement" => ccCount += value+1
            case "ForInStatement" => ccCount += value+1
            case "ForOfStatement" =>  ccCount += value+1
            case "ConditionalExpression" => ccCount += value+1
            case "SwitchStatement" => ccCount += value+1
            case "WhileStatement" => ccCount += value+1
            case "DoWhileStatement" => ccCount += value+1
            case "CatchClause" => ccCount += value+1
            case "FunctionExpression" =>
            case "ArrowFunctionExpression" =>
            case "FunctionDeclaration" =>
          }


        }

        for((ok,ov) <- mapIfOperators) {
          firstOperator = ""
          secondOperator = ""
          if(ov.size == 1) {ccCount += 1}
          else if(ov.size > 1) {
            firstOperator = ov(0)
            ccCount += 1
            for ( i <- 1 to ov.size-1) {
              secondOperator = ov(i)
              if(!secondOperator.equals(firstOperator)) {
                ccCount += 1
              }
              firstOperator = secondOperator
            }
          }
        }


        mapfunctionCC += (k -> ccCount)

      }



      mapFunctionIsRecursion.get(k) match {
        case Some(value) => if(value == 1) ccCount += 1
        case None =>
      }





    }


    if(mapfunctionCC.size > 0) {
      AvgCognitiveComplexity = mapfunctionCC.valuesIterator.reduceLeft(_+_) / mapfunctionCC.size
      LargestCognitiveComplexity = mapfunctionCC.valuesIterator.max
    }



  }


  def computeFFMetrics: Future[Map[String, Any]] = Future {


    computeFanInFanOut
    computeCognitiveComplexity

    Map("HighestFanIn" -> HighestFanIn, "HighestFanOut" -> HighestFanOut, "AvgFanIn" -> AvgFanIn,
      "AvgFanOut" -> AvgFanOut , "LargestCognitiveComplexity" -> LargestCognitiveComplexity, "AvgCognitiveComplexity" -> AvgCognitiveComplexity )
  }

}


