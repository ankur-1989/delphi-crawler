/**
  * Credits: - https://github.com/SrTobi/escalima
  * Scala wrapper of Esprima - http://esprima.org/
  */

package de.upb.cs.swt.delphi.crawler.tools

import javax.script.{Invocable, ScriptEngineManager, ScriptException}


class PrimaJsParserImpl extends PrimaJsParser {

  private val engine = new ScriptEngineManager().getEngineByName("js")
  assert(engine != null, "Could not initialize JavaScript engine")
  private val esprimaObject = {
    val esprimaCode =  scala.io.Source.fromResource("esprima.js").reader()
    engine.eval(esprimaCode)
    engine.get("esprima")
  }
  private val optionObj = engine.eval("var _options = { loc: true, comment: true}; _options;")

  override def parse(source: String, module: Boolean): String = {
    val method = if (module) "parseModule" else "parseScript"
    try {
      val res = engine.asInstanceOf[Invocable].invokeMethod(esprimaObject, method, source, optionObj)
      val json = engine.asInstanceOf[Invocable].invokeMethod(engine.get("JSON"), "stringify", res)
      json.asInstanceOf[String]
    } catch {
      case e: ScriptException =>
        throw PrimaJsParserException(e.getMessage)
    }
  }
}