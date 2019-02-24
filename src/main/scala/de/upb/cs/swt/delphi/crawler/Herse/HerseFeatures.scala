package de.upb.cs.swt.delphi.crawler.Herse

trait HerseFeatures {



  // Features related to complexity of the code
  var SingleLineComments = 0
  var MultiLineComments = 0
  var Ploc = 0
  var NoOfFunctionsDeclarations = 0
  var LargestSignatureInFunction = 0
  var NoofStatementsInLargestFunction = 0
  var AvgNoOfStatementsInFunction = 0

  // Halstead Metrics
  var TotalNoOfOperators: Double = 0
  var TotalNoOfOperands : Double= 0
  var NoOfUniqueOperators : Double = 0
  var NoOfUniqueOperands : Double = 0
  var HalsteadProgramLength : Double = 0
  var HalsteadProgramVolume : Double = 0
  var HalsteadProgramEffort: Double = 0
  var HalsteadDifficulty : Double = 0

  //ECMAScript Compliant Features
  var ES2018Compliant : Boolean = false
  var ES2017Compliant : Boolean = false
  var ES2016Compliant : Boolean = false
  var ES2015Compliant : Boolean = false

  //Cyclomatic complexity
  var HighestCyclomaticComplexity : Double = 0
  var AvgCyclomaticComplexity : Double = 0
  var MaintainabilityIndex: Double = 0

  //Fan -In FanOut Feature
  var FanIn : Double = 0
  var FanOut: Double = 0

}
