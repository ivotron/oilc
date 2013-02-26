package edu.ucsc.oilc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ExampleSuite extends FunSuite with ShouldMatchers {

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
        assert(r.foreachClause.size == 2)
        assert(r.foreachWhereClause.size == 1)
        assert(r.existsClause.size == 3)
        assert(r.existsWhereClause.size == 1)
        assert(r.withClause.size == 3)

        r.foreachClause should contain key ("c")
        assert(r.foreachClause("c").dbObject.name == "companies")
        r.foreachClause should contain key ("g")
        assert(r.foreachClause("g").dbObject.name == "grants")
        //...
      case x =>
        fail(x.toString)
    }
  }

  test("semantic error detection") {
    val p = new SchemaMappingParser


    // lhs foreachWhere clause
    var spec =
      """
      foreach c in companies, g in grants
      where d.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where f.finId = fn.finId
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }

    // lhs existsWhere clause
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where g.finId = fn.finId
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }

    // lhs with clause
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where o.finId = fn.finId
      with c.name = o.code and d.gid = f.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }

    // rhs foreachWhere clause
    spec =
      """
      foreach c in companies, g in grants
      where c.name = fn.recipient
      exists o in organizations, f in fundings, fn in finances
      where o.finId = fn.finId
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }

    // rhs existsWhere clause
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where o.finId = c.finId
      with c.name = o.code and g.gid = f.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }

    // rhs with clause
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, fn in finances
      where o.finId = f.finId
      with c.name = o.code and g.gid = d.fid and g.amount = fn.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }
  }
}
