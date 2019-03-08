package de.upb.cs.swt.delphi.crawler.Herse

import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._
import au.com.bytecode.opencsv.CSVWriter

import scala.collection.mutable.ListBuffer

class CsvWriter(projectName: String, records: Map[String,Any]) {

  val  csvFile = new BufferedWriter(new FileWriter("src/main/resources/HerseResults.csv",true))

  val writer = new CSVWriter(csvFile)
  var recordList = new ListBuffer[Array[String]]()
  var row : Array[String] = Array()
  var fields : Array[String] = Array()

  for(key <- records.keys.toList) {
    fields +:= key
    row +:= records.get(key).get.toString
  }
  fields +:= "projectName"
  row +:= projectName


  //recordList += fields

  recordList += row

  writer.writeAll(recordList.toList)
  csvFile.close()

}
