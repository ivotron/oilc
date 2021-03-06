package edu.ucsc.oilc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class SchemaMappingParserTest extends FunSuite with ShouldMatchers {

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
        assert(r.foreachClause("c").name == "companies")
        r.foreachClause should contain key ("g")
        assert(r.foreachClause("g").name == "grants")
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

    // same identifier twice in foreach
    spec =
      """
      foreach c in companies, c in grants
      where c.name = c.recipient
      exists o in organizations, f in fundings, fn in finances
      where f.finId = fn.finId
      with c.name = o.code and c.gid = f.fid and c.amount = fn.budget
      """

    // TODO: this should throw an exception
    //intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    //}

    // same identifier twice in exists
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, o in finances
      where f.finId = o.finId
      with c.name = o.code and g.gid = f.fid and g.amount = o.budget
      """

    // TODO: this should throw an exception
    //intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    //}

    // same alias used twice in foreach and exists
    spec =
      """
      foreach c in companies, g in grants
      where c.name = g.recipient
      exists o in organizations, f in fundings, g in finances
      where o.finId = f.finId
      with c.name = o.code and g.gid = f.fid and g.amount = g.budget
      """

    intercept[ParseException] {
      p.parseAll(p.schemaMapping, spec) match {
        case p.Success(r,_) => r
        case x => fail(x.toString)
      }
    }
  }

  test("equalities and hashes") {
    val dbo1 = new DBObject("column1", "table1")
    val dbo2 = new DBObject("column1", "table1")

    assert(dbo1 == dbo2)
    assert(dbo1.hashCode == dbo2.hashCode)
  }

}
