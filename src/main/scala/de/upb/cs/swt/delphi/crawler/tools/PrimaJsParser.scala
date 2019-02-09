/**
  * Scala Wrapper of Esprima
  * http://esprima.org/
  * @author Ankur Gupta
  */

package de.upb.cs.swt.delphi.crawler.tools

trait PrimaJsParser {

  def parse(source: String, module: Boolean): String

}
