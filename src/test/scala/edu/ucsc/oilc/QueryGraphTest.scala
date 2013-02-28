package edu.ucsc.oilc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

class QueryGraphTest extends FunSuite with ShouldMatchers {

  /**
   */
  test("basic graph functions") {
    val p = new SchemaMappingParser
    val spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where f.finId = fn.finId and o.oid = f.oid
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    p.parseAll(p.schemaMapping, spec) match {
      case p.Success(r,_) =>
        val graph = QueryGraph.generateGraph(r)

        val o = new DBObject("organizations")
        val f = new DBObject("fundings")
        val fn = new DBObject("finances")
        val c = new DBObject("companies")
        val g = new DBObject("grants")
        val c_name = new DBObject("name", "c")
        val f_fid = new DBObject("fid", "f")
        val f_finId = new DBObject("finId", "f")
        val f_oId = new DBObject("oid", "f")
        val fn_finId = new DBObject("finId", "fn")
        val fn_budget = new DBObject("budget", "fn")
        val g_gid = new DBObject("gid", "g")
        val g_amount = new DBObject("amount", "g")
        val g_recipient = new DBObject("recipient", "g")
        val o_oId = new DBObject("oid", "o")
        val o_code = new DBObject("code", "o")

        assert(graph.order == 16) // 16 nodes
        assert(graph.graphSize == 18) // 18 edges

        // relation nodes
        assert(graph.contains(c))
        assert(graph.contains(g))
        assert(graph.contains(o))
        assert(graph.contains(f))
        assert(graph.contains(fn))

        // source relations -> source columns
        assert(graph.contains(c ~> c_name))
        assert(graph.contains(g ~> g_recipient))
        assert(graph.contains(g ~> g_gid))
        assert(graph.contains(g ~> g_amount))

        // target relations -> target columns
        assert(graph.contains(fn ~> fn_finId))
        assert(graph.contains(fn ~> fn_budget))
        assert(graph.contains(f ~> f_oId))
        assert(graph.contains(f ~> f_finId))
        assert(graph.contains(f ~> f_fid))
        assert(graph.contains(o ~> o_oId))
        assert(graph.contains(o ~> o_code))

        // source columns -> target columns (in WITH clause)
        assert(graph.contains(o_code ~> c_name))
        assert(graph.contains(f_fid ~> g_gid))
        assert(graph.contains(fn_budget ~> g_amount))

        // target columns -> target columns (in WHERE clause)
        assert(graph.contains(f_finId ~> fn_finId))
        assert(graph.contains(fn_finId ~> f_finId ))
        assert(graph.contains(f_oId ~> o_oId))
        assert(graph.contains(o_oId ~> f_oId))

      case x =>
        fail(x.toString)
    }
  }
}
