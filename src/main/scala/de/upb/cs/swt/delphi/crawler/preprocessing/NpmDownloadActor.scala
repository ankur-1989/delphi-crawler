
/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.preprocessing



import scala.util.{Failure, Success}
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import de.upb.cs.swt.delphi.crawler.tools.HttpDownloader

class NpmDownloadActor extends Actor with ActorLogging{

  override def receive: Receive = {
    case m: NpmIdentifier => {
      implicit val system: ActorSystem = context.system

      val downloader = new HttpDownloader()

      val zipStream = downloader.downloadFromUri(m.toZipLocation.toString())

      zipStream match {
        case Success(zip) => {
           log.info(s"Downloaded ${m}")
            sender() ! Success(NpmPackage(m,ZipFile(zip,m.toZipLocation.toURL)))
        }
        case Failure(e) => {
          log.warning(s"Failed zip download for ${m}")
          sender() ! Failure(e)
        }

      }

    }
  }

}

object NpmDownloadActor {
  def props = Props(new NpmDownloadActor)
}
