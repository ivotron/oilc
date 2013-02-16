package edu.ucsc.oilc

import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  test("basic parsing test") {
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
        assert(r.foreach.size == 2)
        assert(r.fwhere.size == 1)
        assert(r.exists.size == 3)
        assert(r.ewhere.size == 1)
        assert(r.withh.size == 3)

        assert(r.foreach(0).id == "c")
        assert(r.foreach(0).relation.obj == "companies")
        assert(r.foreach(1).id == "g")
        assert(r.foreach(1).relation.obj == "grants")
        //...
      case x =>
        fail(x.toString)
    }
  }

}
