package de.upb.cs.swt.delphi.crawler.processing

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.LoggingAdapter
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmPackage
import de.upb.cs.swt.delphi.crawler.escalima.ECMAScript


import scala.util.Try

class HerseActor() extends Actor with ActorLogging with HerseFunctionality  {

  private implicit val l : LoggingAdapter = log

   def receive: PartialFunction[Any,Unit] =  {

    case n: NpmPackage => {


      log.info(s"Starting computing metrics for ${n}")
      val herseResult = Try{
                     computeHerseResults(n)
      }

      sender() ! herseResult
    }

  }

}

object HerseActor {
  def props(): Props = Props(new HerseActor)

}

case class HerseResults(identifier: NpmIdentifier, featureMap: Map[String, Int])


