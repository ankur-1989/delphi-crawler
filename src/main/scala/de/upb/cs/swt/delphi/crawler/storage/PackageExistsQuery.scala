package de.upb.cs.swt.delphi.crawler.storage

import com.sksamuel.elastic4s.http.{ElasticClient, RequestSuccess}
import de.upb.cs.swt.delphi.crawler.discovery.npm.NpmIdentifier
import com.sksamuel.elastic4s.http.search.SearchResponse
import com.sksamuel.elastic4s.http.ElasticDsl._

/**
  * This is to identify if npm package already exists in the elastic search database.
  *
  * @author Ankur Gupta
  */

trait PackageExistsQuery {

  def exists(identifier: NpmIdentifier)(implicit client: ElasticClient) :Boolean = {

    client.execute {
      searchWithType(delphiProjectType) query must (
        matchQuery("name",identifier.toUniqueString)
      )
    }.await match {
      case RequestSuccess(_,_,_,SearchResponse(_, false, false, _, _, _, _, hits)) => (hits.total > 0)
      case _ => false
    }

  }

}
