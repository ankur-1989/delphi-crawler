package de.upb.cs.swt.delphi.crawler.processing

import java.io.{BufferedInputStream, File}
import java.net.URL
import java.util.jar.JarInputStream

import akka.actor.{Actor, ActorLogging, Props}
import de.upb.cs.swt.delphi.crawler.Configuration
import de.upb.cs.swt.delphi.crawler.discovery.maven.MavenIdentifier
import de.upb.cs.swt.delphi.crawler.preprocessing.MavenDownloader
import de.upb.cs.swt.delphi.crawler.processing.CallGraphStream.{MappedEdge, unresMCtoStr}
import de.upb.cs.swt.delphi.crawler.tools.ClassStreamReader
import org.apache.commons.io.FileUtils
import org.opalj.ai.analyses.cg.UnresolvedMethodCall
import org.opalj.br.analyses.Project

/*
 * This class uses static analysis to match unresolved method calls to dependencies using Maven.
 *
 * All dependencies are loaded into OPAL, then all each method is tested to see if it resolves in that
 * dependency's scope. If it does, it is marked as belonging to that dependency.
 */

class MavenEdgeMappingActor(configuration: Configuration) extends Actor with ActorLogging{
  override def receive: Receive = {
    case (mx: Set[UnresolvedMethodCall], ix: Set[MavenIdentifier]) => {
      try {
        sender() ! matchEdges(mx, ix)
      } catch {
        case e: Exception => {
          log.warning("Maven mapper threw exception " + e)
          sender() ! akka.actor.Status.Failure(e)
        }
      }
    }
  }

  private def matchEdges(edgeSet: Set[UnresolvedMethodCall], mavenSet: Set[MavenIdentifier]): Set[MappedEdge] = {

    def edgeSearch(edgeSet: Set[UnresolvedMethodCall], mavenList: List[MavenIdentifier]): Set[MappedEdge] = {
      if (edgeSet.isEmpty){
        Set[MappedEdge]()
      } else {
        if (mavenList.isEmpty) {
          log.info("The following unresolved methods could not be mapped to any library:")
          edgeSet.foreach(m => log.info(unresMCtoStr(m)))
          Set[MappedEdge]()
        } else {
          try {
            val identifier: MavenIdentifier = mavenList.head
            val library = loadProject(identifier)
            val splitSet = edgeSet.partition(m => library.resolveMethodReference(m.calleeClass, m.calleeName, m.calleeDescriptor).isDefined)
            val mappedEdges = splitSet._1.map(m => MappedEdge(identifier, unresMCtoStr(m)))
            mappedEdges ++ edgeSearch(splitSet._2, mavenList.tail)
          } catch {
            case e: java.io.FileNotFoundException => {
              log.info("The Maven coordinates '{}' (listed as a dependency) are invalid", mavenList.head.toString)
              edgeSearch(edgeSet, mavenList.tail)
            }
            case e: java.lang.IllegalArgumentException => {
              log.info("The Maven coordinates '{}' (listed as a dependency) could not be interpreted", mavenList.head.toString)
              edgeSearch(edgeSet, mavenList.tail)
            }
            case e: Exception => {
              log.info("The analysis of dependency {} threw exception {}", mavenList.head.toString, e)
              throw e
            }
          }
        }
      }
    }

    def loadProject(identifier: MavenIdentifier) = {
      val jarFile = new MavenDownloader(identifier).downloadJar()
      val project = new ClassStreamReader {}.createProject(jarFile.url, new JarInputStream(jarFile.is))
      project
    }

    edgeSearch(edgeSet, mavenSet.toList)
  }
}

object MavenEdgeMappingActor {
  def props(configuration: Configuration) = Props(new MavenEdgeMappingActor(configuration))
}
