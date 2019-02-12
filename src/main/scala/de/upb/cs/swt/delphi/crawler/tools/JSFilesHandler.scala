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
          case Some(value) => val newFile = new File(s"src/main/resources/repo/${identifier}/" + value.toString().substring(value.toString().lastIndexOf("/") + 1))
            val bfos = new BufferedOutputStream(new FileOutputStream(newFile))
            val content = new Array[Byte](tarEntry.getSize.toInt)
            var len = tarchive.read(content, 0, tarEntry.getSize.toInt)
            while (len != -1) {
              bfos.write(content, 0, len)
              len = tarchive.read(content, 0, content.length - 0)
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
      case true => println(targetFile.toString)
        FileUtils.deleteDirectory(targetFile)

      case false =>
    }
  }

  def getTargetFileContent(npmIdentifier: String): String = {

    val identifier = npmIdentifier.replace(":","-")
    val directory = "src/main/resources/repo/" + identifier + "/"
    var targetFile = new File(directory + npmIdentifier.substring(0,npmIdentifier.lastIndexOf(":")) + ".js")
    val pattern = """"main":(.*?),""".r
    var content: String = ""
    targetFile.exists() match {
      case true =>

        content = Source.fromFile(targetFile).getLines().mkString

      case false => val jsonFile = new File(directory + "package.json")
        val jsonData = Source.fromFile(jsonFile).getLines().mkString
        pattern.findFirstMatchIn(jsonData) match {
          case Some(value) => targetFile = new File(directory + value.group(1).substring(value.group(1).lastIndexOf("/") + 1).dropRight(1))

            if (targetFile.exists()) {
              content = Source.fromFile(targetFile).getLines().mkString

            }
          case None =>
        }


    }
    content
  }
}