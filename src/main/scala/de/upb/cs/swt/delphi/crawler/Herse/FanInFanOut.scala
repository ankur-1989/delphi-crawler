package de.upb.cs.swt.delphi.crawler.Herse


import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FanInFanout(ast: String) extends HerseFeatures with AstTraverse {

  implicit val format = DefaultFormats

  var functionList: List[String] = List()
  var mapFunctionCallees: Map[String,List[String]] = Map()
  var functionName: String = ""
  var totalFanOut = 0
  var fanIn = 0
  var totalFanIn = 0
  def computeFanInFanOut: Future[Map[String,Any]] = Future {

    getObjectIndexes(ast, "{\"type\":\"FunctionDeclaration\"")
    getObjectIndexes(ast, "{\"type\":\"AssignmentExpression\"")
    getObjectIndexes(ast, "{\"type\":\"VariableDeclarator\"")
    getObjectIndexes(ast, "{\"type\":\"ArrowFunctionExpression\"")
    getObjectIndexes(ast, "{\"type\":\"FunctionExpression\"")

    for ((k, v) <- functionsMap) {


      val closingIndex = findClosingIndex(ast, k,"Function")
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
                case Some(callee) => callee.children.foreach(f =>
                  (f \ "type").toOption match {
                    case Some(t) => if (t.values.toString.contains("MemberExpression"))
                      functionList = f.extract[CalleeMemberExpression].`object` match {
                        case Some(n) => n.name + "." + f.extract[CalleeMemberExpression].property.name :: functionList
                        case None => f.extract[CalleeMemberExpression].property.name :: functionList
                      }
                    else if (t.values.toString.contains("Identifier"))
                      functionList = f.extract[CalleeIdentifierExpression].name :: functionList
                  }

                )
                case None =>
              }



            }
          case None =>
        }
        mapFunctionCallees = mapFunctionCallees ++ Map(functionName -> functionList.distinct)

      }
      else if (closingIndex > 0 && v.contains("FunctionDeclaration")) {

        functionName = obj.extract[FunctionDeclaration].id.name

      }
      else if (closingIndex > 0 && v.contains("VariableDeclarator")) {


        (obj \ "init" \ "type").toOption match {
          case Some(fe) => if (fe.values.toString.contains("FunctionExpression") || fe.values.toString.contains("ArrowFunctionExpression")) {
            functionName = obj.extract[VariableDeclarator].id.name
          }
          case None =>
        }


      }
    }


    for((k,v) <- mapFunctionCallees) {

      if(v.size > HighestFanOut)
        HighestFanOut = v.size

      totalFanOut  = totalFanOut + v.size

      for((fname,callees) <- mapFunctionCallees) {

        if(callees.contains(k)){
          fanIn = fanIn + 1
        }

      }

      if(fanIn > HighestFanIn) {
        HighestFanIn = fanIn
      }

      totalFanIn = totalFanIn + fanIn

    }

    AvgFanOut = totalFanOut / mapFunctionCallees.size
    AvgFanIn = totalFanIn / mapFunctionCallees.size


    Map("HighestFanIn" -> HighestFanIn , "HighestFanOut" -> HighestFanOut , "AvgFanIn" -> AvgFanIn, "AvgFanOut" -> AvgFanOut  )
  }


}


