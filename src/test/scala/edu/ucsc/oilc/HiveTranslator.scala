package edu.ucsc.oilc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

class HiveTranslatorTest extends FunSuite with ShouldMatchers {

  test("basic translation") {
    val p = new SchemaMappingParser
    val spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where f.finId = fn.finId
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    p.parseAll(p.schemaMapping, spec) match {
      case p.Success(r,_) =>
        val g = QueryGraph.generateGraph(r)
        QueryGraph.annotateGraph(g)
        println(QueryGraph.toDotGraph(g))
        println(HiveTranslator.translate(r, g))
      case x =>
        fail(x.toString)
    }
  }
}
