package de.upb.cs.swt.delphi.crawler

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.sksamuel.elastic4s.http.{ElasticClient, HttpClient}
import de.upb.cs.swt.delphi.crawler.control.Server
import de.upb.cs.swt.delphi.crawler.discovery.maven.MavenCrawlActor
import de.upb.cs.swt.delphi.crawler.discovery.maven.MavenCrawlActor.Start
import de.upb.cs.swt.delphi.crawler.preprocessing.PreprocessingDispatchActor
import de.upb.cs.swt.delphi.crawler.processing.{HermesActor, HermesAnalyzer, ProcessingDispatchActor}
import de.upb.cs.swt.delphi.crawler.storage.ElasticActor
import de.upb.cs.swt.delphi.crawler.tools.OPALLogAdapter
import org.opalj.log.{GlobalLogContext, OPALLogger}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * The starter for Delphi Crawler
  */
object Crawler extends App with AppLogging {
  private val configuration = new Configuration()

  implicit val system : ActorSystem = ActorSystem("delphi-crawler")
  implicit val materializer = ActorMaterializer()

  OPALLogger.updateLogger(GlobalLogContext, OPALLogAdapter)
  HermesAnalyzer.setConfig()

  sys.addShutdownHook(() => {
    log.warning("Received shutdown signal.")
    val future = system.terminate()
    Await.result(future, 120.seconds)
  })


  Startup.showStartupInfo
  Startup.preflightCheck(configuration) match {
    case Success(c) =>
    case Failure(e) => {
      system.terminate()
      sys.exit(1)
    }
  }


  log.info("Preflight checks completed. Entering flight mode...")

  new Server(configuration.controlServerPort).start()

  val elasticActor = system.actorOf(ElasticActor.props(ElasticClient(configuration.elasticsearchClientUri)))
  val hermesActor = system.actorOf(HermesActor.props(elasticActor))
  val processingDispatchActor = system.actorOf(ProcessingDispatchActor.props(hermesActor))
  val preprocessingDispatchActor = system.actorOf(PreprocessingDispatchActor.props(configuration, processingDispatchActor, elasticActor))
  val mavenCrawlActor = system.actorOf(MavenCrawlActor.props(configuration, preprocessingDispatchActor))

  mavenCrawlActor ! Start

}
