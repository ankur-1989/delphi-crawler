package de.upb.cs.swt.delphi.crawler.Herse

import org.json4s.JValue

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

    (jsonAst \ "body" \ "declarations").toOption match {
      case Some(value) => value.children.iterator.foreach(f => checkOperands(f))
      case None =>
    }


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
    Map("TotalNoOfOerands" -> TotalNoOfOperands,  "TotalNoOfOperators" -> TotalNoOfOperators,
      "HalsteadProgramLength" -> HalsteadProgramLength , "HalsteadProgramVolume" -> HalsteadProgramVolume , "HalsteadDifficulty" -> HalsteadDifficulty,
      "HalsteadProgramEffort" -> HalsteadProgramEffort)
  }

}
