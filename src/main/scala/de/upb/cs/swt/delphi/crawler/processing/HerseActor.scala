package de.upb.cs.swt.delphi.crawler.processing

import akka.actor.{Actor, ActorLogging, Props}
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage

import scala.util.Try

class HerseActor() extends Actor with ActorLogging with HerseFunctionality  {

   def receive: PartialFunction[Any,Unit] =  {

    case n: NpmPackage => {
      log.info(s"Starting computing metrics for ${n}")
      val herseResult = Try{
                     computeHerseMetrics(n)
      }

      sender() ! herseResult
    }

  }

}

object HerseActor {
  def props(): Props = Props(new HerseActor)

}

case class HerseResults(identifier: NpmIdentifier, featureMap: Map[String, Int])
