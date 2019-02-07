/**
  * @author Ankur Gupta
  *
  */
package de.upb.cs.swt.delphi.crawler.discovery.npm

import java.net.{URI, URL}

import sys.process._
import akka.NotUsed
import akka.event.LoggingAdapter
import akka.stream.scaladsl.RestartSource
import akka.stream.scaladsl.Source
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.util.Try

trait IndexProcessing {

  def createSource(base:URI,npmIndexScript: String)(implicit log:LoggingAdapter) : Source[NpmIdentifier,NotUsed] = {

    log.info("Creating source")

    RestartSource.withBackoff(
      minBackoff = 30.seconds,
      maxBackoff = 90.seconds,
      randomFactor = 0.2, // adds 20% "noise" to vary the intervals slightly
      maxRestarts = 20   // limits the amount of restarts to 20
    ) { () => {
      val ir = Try(new NpmIndexReader(base.toURL,npmIndexScript))
      ir match {
        case scala.util.Success(indexReader) => {
          Source.unfoldResource[NpmIdentifier,NpmIndexReader](
            () => indexReader,
            reader => reader.read(),
            reader => reader.close()
          )
        }
        case scala.util.Failure(exception) => {


          log.error(s"Could not get the package name. Some problem with the $npmIndexScript.")
          throw exception
        }
      }

    }

    }

  }


}


class NpmIndexReader(base: URL,npmIndexScript: String) {
  val log = LoggerFactory.getLogger(this.getClass)
  log.info(s"New npm index reader create for $base")

  val packageString = s"node $npmIndexScript".!!
  val npmPackages = packageString.split("\n")

  lazy val packageIterator = npmPackages.iterator

  def read() : Option[NpmIdentifier] = {

    def readInternal(value: String) = {

      val version = (s"npm view $value version".!!).mkString.trim
      val npmId = NpmIdentifier(base.toString,value,version)
      Some(npmId)

    }

    packageIterator.hasNext match {
      case true =>  Iterator.continually(readInternal(packageIterator.next()))
                              .takeWhile(result => packageIterator.hasNext).collectFirst[NpmIdentifier]({ case Some(x) => x})
      case false => None
    }
  }

  def close() = {

  }

}