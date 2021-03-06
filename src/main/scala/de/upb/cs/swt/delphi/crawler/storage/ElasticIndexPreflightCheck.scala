package de.upb.cs.swt.delphi.crawler.storage

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.{ElasticClient, HttpClient, RequestFailure, RequestSuccess}
import de.upb.cs.swt.delphi.crawler.{Configuration, PreflightCheck}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object ElasticIndexPreflightCheck extends PreflightCheck with ElasticIndexMaintenance {
  override def check(configuration: Configuration)(implicit system: ActorSystem): Try[Configuration] = {
    implicit val ec : ExecutionContext = system.dispatcher
    lazy val client = ElasticClient(configuration.elasticsearchClientUri)

    val f = client.execute {
      indexExists("delphi")
    } andThen {
      case _ => client.close()
    }
    val delphiIndexExists = Await.result(f, Duration.Inf)

    delphiIndexExists match {
      case RequestSuccess(404, _, _, _) => createDelphiIndex(configuration)
      case RequestSuccess(_,_,_,_) => {
        isIndexCurrent(configuration) match {
          case true => Success(configuration) // This is fine
          case false => migrateIndex(configuration) // This needs some work
        }
      }
      case RequestFailure(_, _, _, e) =>  Failure(new ElasticException(e))
    }
  }
}
