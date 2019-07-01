package de.upb.cs.swt.delphi.crawler.Herse

trait HerseFeatures {


  // //Size Metrics
  var SingleLineComments = 0
  var MultiLineComments = 0
  var Ploc = 0
  var Lloc = 0
  var fileSizeinMB : Double = 0.0
  var Sloc = 0

  // //Function Related metrics
  var NoOfFunctionsDeclarations = 0
  var NoOfFunctionExpressions = 0
  var NoOfArrowFunctionExpressions = 0
  var LargestSignatureInFunction = 0
  var NoofStatementsInLargestFunction = 0
  var AvgNoOfStatementsInFunction = 0
  // ES6 Generators
  var NoOfGeneratorFunctions = 0

  // //Complexity Metrics

  // Halstead Metrics
  var TotalNoOfOperators: Double = 0
  var TotalNoOfOperands : Double= 0
  var NoOfUniqueOperators : Double = 0
  var NoOfUniqueOperands : Double = 0
  var HalsteadProgramLength : Double = 0
  var HalsteadProgramVolume : Double = 0
  var HalsteadProgramEffort: Double = 0
  var HalsteadDifficulty : Double = 0

  //Cyclomatic complexity
  var AvgCyclomaticComplexity : Double = 0
  var HighestCyclomaticComplexity: Double = 0
  var MaintainabilityIndex: Double = 0
  //Fan -In FanOut Feature
  var HighestFanIn : Int = 0
  var HighestFanOut: Int = 0
  var AvgFanOut: Double = 0
  var AvgFanIn : Double = 0
  //Cognitive Complexity

  var LargestCognitiveComplexity : Double = 0
  var AvgCognitiveComplexity : Double = 0


  // //ECMAScript Compliant Features
  var ES2018Compliant  = 0
  var ES2017Compliant  = 0
  var ES2016Compliant  = 0
  var ES2015Compliant  = 0
  var ES2019Compliant = 0




  // JavaScript Objects
  var NoOfObjects: Int = 0
  var  NoOfPropertiesInLargestObject : Int = 0
  var AvgNoOfPropertiesPerObject: Int = 0
  var NoOfAssignmentExpressions = 0
  var NoOfObjectInstancesSites = 0





  var LongestCallingChain: Double = 0
  var AvgCallingChain : Double = 0

  //JavaScript Reflection Metrics

  var NoOfInstanceOfOperator = 0
  var NoOfTypeOfOperator = 0
  var NoOfReflectMethods = 0
  var NoOfEvalUsages = 0
  var NoOfWithStatementUsages = 0



  // Js Module types
  var CommonJsModuleType = 0
  var AMDModuleType = 0
  var ES6ModuleType = 0
  var UMDModuleType = 0


  // Asynchronous Metrics
  var NoOfAsyncFunctions = 0
  var NoOfAwaitExpressions = 0
  var NoOfTimerStatements = 0

  // Javascript Class Metrics
  var NoOfClassDeclarations = 0
  var AvgNoOfMethodsPerClass : Double =  0
  var HighestNoOfMethodsPerClass = 0

  //JavaScript RegEx Metrics
  var NoOfRegExpressionUsages = 0

}
