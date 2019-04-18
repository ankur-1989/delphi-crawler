package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.DefaultFormats
import org.json4s.native.Serialization._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.JsonMethods._

import scala.collection.immutable.ListMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FanInFanout(ast: String, createObjectMaps: CreateObjectMaps) extends HerseFeatures with AstTraverse {

  implicit val format = DefaultFormats


  var functionName: String = ""
  var totalFanOut = 0
  var fanIn = 0
  var totalFanIn = 0
  var ccCount = 0
  var mapfunctionCC = scala.collection.mutable.Map[Int, Int]()
  var mapFunctionIsRecursion = scala.collection.mutable.Map[Int, Int]()
  var mapIfOperators = scala.collection.mutable.Map[Int, List[String]]()
  var mapFunctionCallees = scala.collection.mutable.Map[String, List[String]]()
  var firstOperator: String = ""
  var secondOperator: String = ""

  def computeFanInFanOut = {

    for ((k, closingIndex) <- ListMap(createObjectMaps.functionIndexMap.toSeq.sortBy(_._1): _*)) {


      var functionList: List[String] = List()
      functionName = ""

      val functionobj = parse(ast.substring(k, closingIndex + 1))
      if (closingIndex > 0 && createObjectMaps.functionsMap.get(k).get.contains("AssignmentExpression")) {

        (functionobj \ "right" \ "type").toOption match {
          case Some(value) =>

            if (value.values.toString.contains("FunctionExpression") || value.values.toString.contains("ArrowFunctionExpression")) {

              (functionobj \ "left" \ "type").toOption match {
                case Some(v) => if (v.values.toString.equals("MemberExpression")) {


                  val left = parse(write(functionobj.extract[AssignmentMemberExpression].left))

                  val nameList = getElement("name", left)
                  var fName = ""
                  for (name <- nameList.asInstanceOf[List[String]]) {
                    fName = fName + name + "."
                  }
                  functionName = fName.substring(0, fName.length - 1)


                } else if (v.values.toString.equals("Identifier")) {
                  functionName = functionobj.extract[AssignmentIdentifierExpression].left.name
                }
                case None =>
              }


              (functionobj \\ "callee").toOption match {
                case Some(callee) => if ("callee".r.findAllIn(compact(render(functionobj))).matchData.toList.size > 1) {

                  callee.children.foreach(f =>
                    (f \ "type").toOption match {
                      case Some(t) => if (t.values.toString.contains("MemberExpression")) {

                        val nameList = getElement("name", f)

                        var fName = ""
                        for (name <- nameList.asInstanceOf[List[String]]) {
                          fName = fName + name + "."
                        }

                        functionList = fName.substring(0, fName.length - 1) :: functionList

                      }
                      else if (t.values.toString.contains("Identifier"))
                        functionList = f.extract[CalleeIdentifierExpression].name :: functionList
                      case None =>
                    }

                  )
                }
                else {
                  callee.children.foreach(f =>
                    (f \ "type").toOption match {
                      case Some(t) => if (t.values.toString.contains("MemberExpression")) {
                        val nameList = getElement("name", f)
                        var fName = ""
                        for (name <- nameList.asInstanceOf[List[String]]) {
                          fName = fName + name + "."
                        }
                        functionList = fName.substring(0, fName.length - 1) :: functionList
                      }
                      else if (t.values.toString.contains("Identifier"))
                        functionList = f.extract[CalleeIdentifierExpression].name :: functionList
                      case None =>
                    }

                  )
                }
                case None =>
              }

              mapFunctionCallees +=  (functionName -> functionList.distinct)
            }
          case None =>
        }


      } else if (closingIndex > 0 && createObjectMaps.functionsMap.get(k).get.contains("FunctionDeclaration")) {

        functionName = functionobj.extract[FunctionDeclaration].id.name


        (functionobj \\ "callee").toOption match {
          case Some(callee) =>
            if ("callee".r.findAllIn(compact(render(functionobj))).matchData.toList.size > 1) {

              callee.children.foreach(f => {

                (f \ "type").toOption match {
                  case Some(t) =>
                    if (t.values.toString.contains("MemberExpression")) {

                      val nameList = getElement("name", f)
                      var fName = ""
                      for (name <- nameList.asInstanceOf[List[String]]) {

                        fName = fName + name + "."
                      }

                      functionList = fName.substring(0, fName.length - 1) :: functionList

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
                    val nameList = getElement("name", callee)
                    var fName = ""
                    for (name <- nameList.asInstanceOf[List[String]]) {
                      fName = fName + name + "."
                    }

                    functionList = fName.substring(0, fName.length - 1) :: functionList
                  }
                  else if (t.values.toString.contains("Identifier")) {

                    functionList = callee.extract[CalleeIdentifierExpression].name :: functionList


                  }
                case None =>

              }
            }

          case None =>
        }

        mapFunctionCallees += (functionName -> functionList.distinct)



      } else if (closingIndex > 0 && createObjectMaps.functionsMap.get(k).get.contains("VariableDeclarator")) {


        (functionobj \ "init" \ "type").toOption match {
          case Some(fe) => if (fe.values.toString.contains("FunctionExpression") || fe.values.toString.contains("ArrowFunctionExpression")) {
            functionName = functionobj.extract[VariableDeclarator].id.name

            (functionobj \\ "callee").toOption match {

              case Some(callee) => if ("callee".r.findAllIn(compact(render(functionobj))).matchData.toList.size > 1) {

                callee.children.foreach(f => {

                  (f \ "type").toOption match {
                    case Some(t) =>
                      if (t.values.toString.contains("MemberExpression")) {

                        val nameList = getElement("name", f)
                        var fName = ""
                        for (name <- nameList.asInstanceOf[List[String]]) {
                          fName = fName + name + "."
                        }
                        functionList = fName.substring(0, fName.length - 1) :: functionList
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
                      val nameList = getElement("name", callee)
                      var fName = ""
                      for (name <- nameList.asInstanceOf[List[String]]) {
                        fName = fName + name + "."
                      }
                      functionList = fName.substring(0, fName.length - 1) :: functionList
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


        mapFunctionCallees += (functionName -> functionList.distinct)


      }



      if (functionList.distinct.contains(functionName)) {


        if ((createObjectMaps.functionsMap.get(k).get.contains("AssignmentExpression") || createObjectMaps.functionsMap.get(k).get.contains("VariableDeclarator"))) {
          for ((key, value) <- ListMap(createObjectMaps.functionsMap.toSeq.sortBy(_._1): _*)) {
            if (key > k && (value.contains("FunctionExpression") || value.contains("ArrowFunctionExpression")) &&
              (createObjectMaps.functionsMap.get(k).get.contains("VariableDeclarator") || createObjectMaps.functionsMap.get(k).get.contains("AssignmentExpression"))) {

              mapFunctionIsRecursion += (key -> 1)
            }
          }
        } else if (createObjectMaps.functionsMap.get(k).get.contains("FunctionDeclaration")) {
          mapFunctionIsRecursion += (k -> 1)
        }
      }


    }


    for ((k, v) <- mapFunctionCallees) {


      if (v.size > HighestFanOut)
        HighestFanOut = v.size

      totalFanOut = totalFanOut + v.size
      fanIn = 0
      for ((fname, callees) <- mapFunctionCallees) {

        if (callees.contains(k)) {
          fanIn = fanIn + 1
        } else {
          if(k.lastIndexOf(".") > 0) {
            val lastPartOfFunction = k.substring(k.lastIndexOf(".")+1)
            callees.foreach( f => {
              if(f.lastIndexOf(".")>0 && f.substring(f.lastIndexOf(".")+1).equals(lastPartOfFunction)) {
                fanIn = fanIn +1
              } else if(f.equals(lastPartOfFunction)) {
                fanIn = fanIn + 1
              }
            })
          }
        }

      }

      if (fanIn > HighestFanIn) {
        HighestFanIn = fanIn
      }

      totalFanIn = totalFanIn + fanIn

    }


    if (mapFunctionCallees.size > 0) {
      AvgFanOut = BigDecimal(totalFanOut.toDouble / mapFunctionCallees.size.toDouble).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      AvgFanIn = BigDecimal(totalFanIn.toDouble / mapFunctionCallees.size.toDouble).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }



  }

  def computeCognitiveComplexity = {


    for ((k, v) <- createObjectMaps.functionIndexMap) {
      ccCount = 0
      typeStatementsMap.clear()
      typeStatementsIndexMap.clear()
      mapStatementsNesting.clear()
      mapIfOperators.clear()
      var calleeList: List[String] = List()


      if (createObjectMaps.functionsMap.get(k).get.equals("FunctionDeclaration") || createObjectMaps.functionsMap.get(k).get.equals("FunctionExpression")
        || createObjectMaps.functionsMap.get(k).get.equals("ArrowFunctionExpression")) {

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

        statements.asInstanceOf[List[String]].filter(f => f.equals("BreakStatement") || f.equals("ContinueStatement")).foreach(m =>
          ccCount += 1
        )

        for ((key, value) <- mapStatementsNesting) {

          typeStatementsMap.get(key).get match {
            case "IfStatement" => var operatorList: List[String] = List()
              """"operator":(.*?),""".r.findAllIn(ast.substring(key, typeStatementsIndexMap.get(key).get)).matchData foreach (f => {
                val operator = f.group(1).replace("\"", "")
                if (operator.equals("||") || operator.equals("&&") || operator.equals("!")) {
                  operatorList = operator :: operatorList
                }

              })
              mapIfOperators += (key -> operatorList)
              ccCount += value + 1
            case "ForStatement" => ccCount += value + 1
            case "ForInStatement" => ccCount += value + 1
            case "ForOfStatement" => ccCount += value + 1
            case "ConditionalExpression" => ccCount += value + 1
            case "SwitchStatement" => ccCount += value + 1
            case "WhileStatement" => ccCount += value + 1
            case "DoWhileStatement" => ccCount += value + 1
            case "CatchClause" => ccCount += value + 1
            case "FunctionExpression" =>
            case "ArrowFunctionExpression" =>
            case "FunctionDeclaration" =>
          }


        }

        for ((ok, ov) <- mapIfOperators) {
          firstOperator = ""
          secondOperator = ""
          if (ov.size == 1) {
            ccCount += 1
          }
          else if (ov.size > 1) {
            firstOperator = ov(0)
            ccCount += 1
            for (i <- 1 to ov.size - 1) {
              secondOperator = ov(i)
              if (!secondOperator.equals(firstOperator)) {
                ccCount += 1
              }
              firstOperator = secondOperator
            }
          }
        }


        mapfunctionCC += (k -> ccCount)

      }


      mapFunctionIsRecursion.get(k) match {
        case Some(value) => if (value == 1) ccCount += 1
        case None =>
      }


    }


    if (mapfunctionCC.size > 0) {
      AvgCognitiveComplexity = mapfunctionCC.valuesIterator.reduceLeft(_ + _) / mapfunctionCC.size
      LargestCognitiveComplexity = mapfunctionCC.valuesIterator.max
    }


  }


  def computeFFMetrics: Future[Map[String, Any]] = Future {


    computeFanInFanOut
    computeCognitiveComplexity

    Map("HighestFanIn" -> HighestFanIn, "HighestFanOut" -> HighestFanOut, "AvgFanIn" -> AvgFanIn,
      "AvgFanOut" -> AvgFanOut, "LargestCognitiveComplexity" -> LargestCognitiveComplexity, "AvgCognitiveComplexity" -> AvgCognitiveComplexity)
  }


}