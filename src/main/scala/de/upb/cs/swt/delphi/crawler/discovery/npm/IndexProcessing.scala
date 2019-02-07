//package de.upb.cs.swt.delphi.crawler.discovery.npm
//
//import java.net.{URI, URL}
//
//import akka.NotUsed
//import akka.event.LoggingAdapter
//import akka.stream.scaladsl.Source
//import org.slf4j.LoggerFactory
//
//trait IndexProcessing {
//
//  def createSource(base:URI)(implicit log:LoggingAdapter) : Source[NpmIdentifier,NotUsed] = {
//
//    log.info("Creating source")
//
//  }
//
//
//}
//
//
//class NpmIndexReader(base: URL) {
//  val log = LoggerFactory.getLogger(this.getClass)
//
//
//}