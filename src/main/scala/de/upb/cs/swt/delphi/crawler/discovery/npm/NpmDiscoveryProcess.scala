/**
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.discovery.npm

import akka.actor.{ActorRef, ActorSystem}
import akka.event.LoggingAdapter
import akka.routing.SmallestMailboxPool
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.Timeout
import akka.pattern.ask
import com.sksamuel.elastic4s.http.ElasticClient
import de.upb.cs.swt.delphi.crawler.control.Phase
import de.upb.cs.swt.delphi.crawler.control.Phase.Phase
import de.upb.cs.swt.delphi.crawler.preprocessing.{NpmDownloadActor, NpmPackage}
import de.upb.cs.swt.delphi.crawler.tools.ActorStreamIntegrationSignals.{Ack, StreamCompleted, StreamFailure, StreamInitialized}
import de.upb.cs.swt.delphi.crawler.tools.NotYetImplementedException
import de.upb.cs.swt.delphi.crawler.{AppLogging, Configuration}
//import de.upb.cs.swt.delphi.crawler.discovery.npm.IndexProcessing
import de.upb.cs.swt.delphi.crawler.storage.PackageExistsQuery
import scala.concurrent.duration._
import scala.collection.mutable
import scala.util.{Failure, Try,Success}

class NpmDiscoveryProcess(configuration:Configuration, elasticPool : ActorRef)(implicit system:ActorSystem)
  extends de.upb.cs.swt.delphi.crawler.control.Process[Long]
    with AppLogging
    with IndexProcessing
    with PackageExistsQuery {

  private val seen = mutable.HashSet[NpmIdentifier]()

  val downloaderPool = system.actorOf(SmallestMailboxPool(8).props(NpmDownloadActor.props))
  //var hersePool Todo Need to create herseactor

  override def phase : Phase = Phase.Discovery

  override def start: Try[Long] = {
    implicit val materializer = ActorMaterializer()
    implicit val logger: LoggingAdapter = log
    implicit val client = ElasticClient(configuration.elasticsearchClientUri)



    var filteredSource = createSource(configuration.npmRepoBase,configuration.npmIndexScript)
      .filter(m => {
        val before = seen.contains(m)
        if (!before) seen.add(m)
        !before
      })
      .filter(m => !exists(m)) // ask elastic
      .throttle(configuration.throttle.element, configuration.throttle.per, configuration.throttle.maxBurst, configuration.throttle.mode)

    if (configuration.limit > 0) {
      filteredSource = filteredSource.take(configuration.limit)
    }

    implicit val timeout = Timeout(5 minutes)

    val preprocessing =
      filteredSource
        .alsoTo(createSinkFromActorRef[NpmIdentifier](elasticPool))
        .mapAsync(8)(identifier => (downloaderPool ? identifier).mapTo[Try[NpmPackage]])
        .filter(npmpackage => npmpackage.isSuccess)
        .map(npmpackage => npmpackage.get)
        .to(Sink.ignore)
        .run()


    Success(0L)

  }

  private def createSinkFromActorRef[T](actorRef: ActorRef) = {
    Sink.actorRefWithAck[T](actorRef, StreamInitialized, Ack, StreamCompleted, StreamFailure)
  }

  override def stop: Try[Long] = {
    Failure(new NotYetImplementedException)
  }

}
