package edu.ucsc.oilc

import scala.util.parsing.combinator._

/** Parses the following:
 *
 * {{{
 * FOR
 *    <var> IN <relationSymbol>,
 *    ...,
 *    <var> IN <relationSymbol>
 * WHERE
 *    <eqCondition> AND
 *    ... AND
 *    <eqCondition>
 * EXISTS
 *    <var> IN <relationSymbol>,
 *    ...,
 *    <var> IN <relationSymbol>
 * WHERE
 *    <eqCondition> AND
 *    ... AND
 *    <eqCondition>
 * WITH
 *    <eqCondition> AND
 *    ... AND
 *    <eqCondition>
 * }}}
 *
 * In the above:
 *  - variables in `FOR` and `EXISTS` clauses correspond to relational symbols, i.e. a variable
 *    "points" to a relation members (a.k.a. columns from tables).
 *  - `FOR` clause refers to relations in the source schema
 *  - `EXISTS` clause refers to relations in the target schema
 *  - equality conditions on the `WHERE` following the `FOR` clause refer to variables on the
 *    source schema, i.e. they're conditions over the source schema
 *  - equality conditions on the `WHERE` following the `EXISTS` clause refer to variables on the
 *    target schema, i.e. they're conditions over the target schema
 *  - conditions on the `WITH` clause relate variables used in both source and target schemas, i.e.
 *    the `FOR` and `EXISTS` clauses.
 */
class SchemaMappingParser extends JavaTokenParsers {

  def schemaMapping =
    foreach ~ where ~ exists ~ where ~ withh ^^ {
      case f ~ fw ~ e ~ ew ~ w => new QueryGraph(f, fw, e, ew, w)
    }

  def foreach =
    "foreach" ~> repsep(relationVariable, ",") ^^ { (relVars: List[RelationVariable]) => relVars }

  def where =
    "where" ~> repsep(equality, "and") ^^ { (eqs: List[Equality]) => eqs }

  def exists =
    "exists" ~> repsep(relationVariable, ",") ^^ { (relVars: List[RelationVariable]) => relVars }

  def withh =
    "with" ~> repsep(equality, "and") ^^ { (eqs: List[Equality]) => eqs }

  //===========

  def equality =
    qualifiedId ~ "=" ~ qualifiedId ^^ { case q1 ~ _ ~ q2 => new Equality(q1, q2) }

  def relationVariable =
    id ~ "in" ~ qualifiedId ^^ { case i ~ _ ~ q => new RelationVariable(i, q) }

  def qualifiedId =
    id ~ "." ~ id ^^ { case c ~ _ ~ o => new Identifier(o, c) } |
    id            ^^ { case o => new Identifier(o) }

  def id =
    ident ^^ { case s => s }
}

class Identifier(val obj: String, val container: String) {
  def this(obj: String) = this(obj, "")
}

class Equality(val id1: Identifier, val id2: Identifier) {
}

class RelationVariable(val id: String, val relation: Identifier) {
}

class QueryGraph(
    val foreach: List[RelationVariable],
    val fwhere: List[Equality],
    val exists: List[RelationVariable],
    val ewhere: List[Equality],
    val withh: List[Equality]) {
}
