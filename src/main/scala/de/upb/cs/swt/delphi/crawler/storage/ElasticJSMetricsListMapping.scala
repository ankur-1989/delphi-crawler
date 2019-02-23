/**
  * @author Ankur Gupta
  *         Stores the metrics returned by Herse in order to store the results in elastic search
  */

package de.upb.cs.swt.delphi.crawler.storage

import com.sksamuel.elastic4s.mappings.FieldDefinition
import com.sksamuel.elastic4s.http.ElasticDsl._

object ElasticJSMetricsListMapping {


  def getMetricsList : Seq[FieldDefinition] = {
    featureMap.toSeq.map{case (name, fun) => fun(name)}
  }

  private val featureMap: Map[String, String => FieldDefinition] = Map[String, String => FieldDefinition](
    "Ploc" -> intField,
    "Lloc" -> intField,
    "SingleLineComments" -> intField,
    "MultiLineComments" -> intField,
    "LargeSignatureInFunction" -> intField,
    "NoOfFunctionsDeclarations" -> intField,
    "TotalNoOfOperators" -> doubleField,
    "TotalNoOfOperands" -> doubleField,
    "NoOfUniqueOperators" -> doubleField,
    "NoOfUniqueOperands" -> doubleField,
    "HalsteadProgramLength" -> doubleField,
    "HalsteadProgramVolume" -> doubleField,
    "HalsteadProgramEffort" -> doubleField,
    "HalsteadDifficulty" -> doubleField,
    "MaintainabilityIndex" -> doubleField,
    "ES2018Compliant" -> booleanField,
    "ES2017Compliant" -> booleanField,
    "ES2016Compliant" -> booleanField,
    "ES2015Compliant" -> booleanField,
    "CyclomaticComplexity" -> doubleField,
    "MaintainabilityIndex" -> doubleField,
    "NoofStatementsInLargestFunction" -> intField,
    "AvgNoOfStatementsInFunction" -> intField
  )

}
