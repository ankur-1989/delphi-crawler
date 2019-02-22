package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s
import org.json4s.JsonAST.JObject
import org.json4s.{JField, JValue}

import scala.concurrent.Future
import scala.math.{BigDecimal, log10}
import scala.concurrent.ExecutionContext.Implicits.global

class HalsteadMetrics(jsonAst: JValue) extends HerseFeatures with AstTraverse {



  var log2 = (num: Double) => log10(num)/log10(2.0)

  def computeTotalOperands()  = {

    val listOperands = getElement("type", jsonAst)
    listOperands.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).get("VariableDeclarator") match {
      case Some(value) => TotalNoOfOperands = value
      case None =>
    }

    getVariables("declarations",jsonAst).asInstanceOf[List[Any]].iterator.foreach( f => f match {
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

    val listOperators = getElement("operator", jsonAst)
    TotalNoOfOperators = listOperators.asInstanceOf[List[String]].size
    NoOfUniqueOperators = listOperators.asInstanceOf[List[String]].groupBy(identity).mapValues(_.size).size

  }



  def computeHalsteadMetrics() : Future[Map[String,Double]] = Future {

    computeTotalOperators()
    computeTotalOperands()
    HalsteadProgramLength =  BigDecimal(TotalNoOfOperators + TotalNoOfOperands).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    HalsteadProgramVolume = BigDecimal(HalsteadProgramLength * log2(NoOfUniqueOperands + NoOfUniqueOperators)).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble
    HalsteadDifficulty = BigDecimal((NoOfUniqueOperators * TotalNoOfOperands)/ (2*NoOfUniqueOperands)).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble
    HalsteadProgramEffort =  BigDecimal( HalsteadDifficulty * HalsteadProgramVolume).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble
    Map("TotalNoOfOperands" -> TotalNoOfOperands,  "TotalNoOfOperators" -> TotalNoOfOperators, "NoOfUniqueOperators" -> NoOfUniqueOperators,
      "NoOfUniqueOperands" -> NoOfUniqueOperands,
      "HalsteadProgramLength" -> HalsteadProgramLength , "HalsteadProgramVolume" -> HalsteadProgramVolume , "HalsteadDifficulty" -> HalsteadDifficulty,
      "HalsteadProgramEffort" -> HalsteadProgramEffort)
  }

}
