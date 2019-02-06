/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.discovery.npm

import akka.actor.{ActorRef, ActorSystem}
import akka.event.LoggingAdapter
import akka.routing.SmallestMailboxPool
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.sksamuel.elastic4s.http.ElasticClient
import de.upb.cs.swt.delphi.crawler.control.Phase
import de.upb.cs.swt.delphi.crawler.control.Phase.Phase
import de.upb.cs.swt.delphi.crawler.preprocessing.NpmDownloadActor
import de.upb.cs.swt.delphi.crawler.tools.ActorStreamIntegrationSignals.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import de.upb.cs.swt.delphi.crawler.tools.NotYetImplementedException
import de.upb.cs.swt.delphi.crawler.{AppLogging, Configuration}
import de.upb.cs.swt.delphi.crawler.discovery.npm.IndexProcessing

import scala.collection.mutable
import scala.util.{Failure, Try}

class NpmDiscoveryProcess(configuration:Configuration, elasticPool : ActorRef)(implicit system:ActorSystem)
  extends de.upb.cs.swt.delphi.crawler.control.Process[Long]
    with AppLogging
    with IndexProcessing {

  private val seen = mutable.HashSet[NpmIdentifier]()

  val downloaderPool = system.actorOf(SmallestMailboxPool(8).props(NpmDownloadActor.props))
  //var hersePool Todo Need to create herseactor

  override def phase : Phase = Phase.Discovery

  override def start: Try[Long] = {
    implicit val materializer = ActorMaterializer()
    implicit val logger: LoggingAdapter = log
    implicit val client = ElasticClient(configuration.elasticsearchClientUri)

    var filteredSource = createSource(configuration.npmRepoBase)

  }

  private def createSinkFromActorRef[T](actorRef: ActorRef) = {
    Sink.actorRefWithAck[T](actorRef, StreamInitialized, Ack, StreamCompleted, StreamFailure)
  }

  override def stop: Try[Long] = {
    Failure(new NotYetImplementedException)
  }

}
