/**
  * @author Ankur Gupta
  *         This is to handle the tgz file and filter the target javascript file for Herse
  */

package de.upb.cs.swt.delphi.crawler.tools

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileOutputStream}
import java.net.URL

import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.FileUtils

import scala.io.Source

trait JSFilesHandler {


  def createJsRepo(npmIdentifier: String, is: BufferedInputStream) = {

    val identifier = npmIdentifier.replace(":","-")

    val tarchive = new TarArchiveInputStream(new GzipCompressorInputStream(is))
    val project = new File(s"src/main/resources/repo/${identifier}/")
    if (!project.exists()) {
      project.mkdir()
    }

    var tarEntry = tarchive.getNextTarEntry
    val pattern = "^((?!min|map).)*\\.(js|json)$".r
    while (tarEntry != null) {

      if (!tarEntry.isDirectory) {
        pattern.findFirstMatchIn(tarEntry.getName) match {
          case Some(value) =>
            val directory = new File(s"src/main/resources/repo/${identifier}/"+ value.toString().substring(0,value.toString().lastIndexOf("/")+1))
            if(!directory.exists()) {
              directory.mkdirs()
            }
            val bfos = new BufferedOutputStream(new FileOutputStream(new File( directory + "/" + value.toString().substring(value.toString().lastIndexOf("/")+1))))
            val buf = new Array[Byte](tarEntry.getSize.toInt)
            var len = tarchive.read(buf, 0, tarEntry.getSize.toInt)
            while (len != -1) {
              bfos.write(buf,  0, len)
              len = tarchive.read(buf, 0, buf.length-0)
            }
            bfos.close()
          case None =>
        }
      }
      tarEntry = tarchive.getNextTarEntry
    }

    tarchive.close()
  }

  def deleteJsRepo(npmIdentifier: String) = {

    val identifier = npmIdentifier.replace(":","-")
    val directory = "src/main/resources/repo/" + identifier + "/"
    val targetFile = new File(directory)

    (targetFile.exists() && targetFile.isDirectory) match {
      case true =>
        FileUtils.deleteDirectory(targetFile)

      case false =>
    }
  }


  def checkFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(checkFiles)
  }


  def getTargetFile(npmIdentifier: String): String = {


    val identifier = npmIdentifier.replace(":","-")
    val directory = "src/main/resources/repo/" + identifier + "/" + "package/"
    val pattern = """"main":(.*?),""".r
    var targetFile = new File("")
    val jsonFile = new File(directory + "package.json")
    val jsonData = Source.fromFile(jsonFile).getLines().mkString
    pattern.findFirstMatchIn(jsonData) match {
      case Some(value) =>  val fileName = value.group(1).substring(0).replaceAll("\"","").trim
        targetFile = new File(directory + fileName.substring(if(fileName.lastIndexOf("/") < 0) 0 else fileName.lastIndexOf("/")+1))

        if (!targetFile.toString.endsWith(".js")) {
          targetFile = new File(targetFile.toString + ".js")
        }
        if (!targetFile.exists()) {
          val it = checkFiles(new File(directory))
            .toList.filter(_.toString.contains(targetFile.toString.substring(targetFile.toString.lastIndexOf("/")+1))).iterator
          if(it.nonEmpty) {
            it.reduceLeft((x,y) => if(x.length() > y.length()) x else y) match {
              case file: File => targetFile = file
            }
          }
        }
      case None =>
    }

    val it = checkFiles(new File(directory))
      .toList.filter(_.toString.contains(npmIdentifier.substring(0,npmIdentifier.indexOf(":"))+".js") == true).iterator
    if (it.nonEmpty) {
      it.reduceLeft((x, y) => if (x.length() > y.length()) x else y) match {
        case file: File => if (!targetFile.toString.equals(file)) {
          targetFile = if (targetFile.length() > file.length()) targetFile else file

        }

      }


    }
    targetFile.toString
  }
}