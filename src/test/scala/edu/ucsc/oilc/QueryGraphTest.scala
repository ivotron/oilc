package edu.ucsc.oilc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

class QueryGraphTest extends FunSuite with ShouldMatchers {

  test("graph structure") {
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

        val root = new DBObject("root")
        val source = new DBObject("source")
        val target = new DBObject("target")
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

        assert(graph.order == 19) // 19 nodes
        assert(graph.graphSize == 25) // 25 edges

        // relation nodes
        assert(graph.contains(root))
        assert(graph.contains(c))
        assert(graph.contains(g))
        assert(graph.contains(o))
        assert(graph.contains(f))
        assert(graph.contains(fn))

        // root to source and targets
        assert(graph.contains(root ~> source))
        assert(graph.contains(root ~> target))

        // source relations
        assert(graph.contains(source ~> c))
        assert(graph.contains(source ~> g))

        // target relations
        assert(graph.contains(target ~> o))
        assert(graph.contains(target ~> f))
        assert(graph.contains(target ~> fn))

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

        // target columns -> source columns (in WITH clause)
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

  test("graph annotations") {
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

        QueryGraph.annotateGraph(graph)

        val root = new DBObject("root")
        val source = new DBObject("source")
        val target = new DBObject("target")
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

        assert(graph.order == 19) // 19 nodes
        assert(graph.graphSize == 25) // 25 edges

        val dot =
"""digraph QueryGraph {
	root -> target
	"fn.finId(Set(g.gid, g.amount, c.name))" -> "f.finId(Set(g.gid, g.amount, c.name))"
	grants -> "g.gid"
	companies -> "c.name"
	"finances(Set(g.gid, g.amount, c.name))" -> "fn.budget(Set(g.amount, g.gid, c.name))"
	source -> companies
	target -> "organizations(Set(g.gid, c.name, g.amount))"
	grants -> "g.recipient"
	"f.oid(Set(g.gid, c.name, g.amount))" -> "o.oid(Set(g.gid, c.name, g.amount))"
	grants -> "g.amount"
	"fundings(Set(g.gid, g.amount, c.name))" -> "f.fid(Set(g.gid, g.amount, c.name))"
	"f.fid(Set(g.gid, g.amount, c.name))" -> "g.gid"
	"finances(Set(g.gid, g.amount, c.name))" -> "fn.finId(Set(g.gid, g.amount, c.name))"
	"o.oid(Set(g.gid, c.name, g.amount))" -> "f.oid(Set(g.gid, c.name, g.amount))"
	"fundings(Set(g.gid, g.amount, c.name))" -> "f.finId(Set(g.gid, g.amount, c.name))"
	target -> "fundings(Set(g.gid, g.amount, c.name))"
	target -> "finances(Set(g.gid, g.amount, c.name))"
	"fundings(Set(g.gid, g.amount, c.name))" -> "f.oid(Set(g.gid, c.name, g.amount))"
	"fn.budget(Set(g.amount, g.gid, c.name))" -> "g.amount"
	"f.finId(Set(g.gid, g.amount, c.name))" -> "fn.finId(Set(g.gid, g.amount, c.name))"
	source -> grants
	"organizations(Set(g.gid, c.name, g.amount))" -> "o.code(Set(c.name, g.gid, g.amount))"
	"o.code(Set(c.name, g.gid, g.amount))" -> "c.name"
	root -> source
	"organizations(Set(g.gid, c.name, g.amount))" -> "o.oid(Set(g.gid, c.name, g.amount))"
}"""
        assert(dot == QueryGraph.toDotGraph(graph))

      case x =>
        fail(x.toString)
    }
  }
}
