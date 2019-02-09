package de.upb.cs.swt.delphi.crawler.processing

import akka.actor.{Actor, ActorLogging, Props}
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage

import scala.util.Try

class HerseActor() extends Actor with ActorLogging {

   def receive: PartialFunction[Any,Unit] =  {

    case n: NpmPackage => {
      log.info(s"Starting computing metrics for ${n}")
      val featureMap = Map("Ploc" -> 50)
      val herseResult = Try{HerseResults(n.identifier,featureMap)}

      sender() ! herseResult
    }

  }

}

object HerseActor {
  def props(): Props = Props(new HerseActor)

}

case class HerseResults(identifier: NpmIdentifier, featureMap: Map[String, Int])
