package edu.ucsc.oilc

import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  test("high-level rule") {
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
        assert(r.toString == "((((List(c, g)~List(c))~List(o, f, fn))~List(f))~List(c, g, g))")
      case x =>
        fail(x.toString)
    }
  }
}
