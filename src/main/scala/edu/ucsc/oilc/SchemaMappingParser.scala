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

  def schemaMapping = FOREACH ~ WHERE ~ EXISTS ~ WHERE ~ WITH

  def FOREACH = "foreach" ~> repsep(relationVariable, ",")

  def WHERE = "where" ~> repsep(equality, "and")

  def EXISTS = "exists" ~> repsep(relationVariable, ",")

  def WITH = "with" ~> repsep(equality, "and")

  //===========

  def equality = qualifiedId <~ "=" ~> qualifiedId

  def relationVariable = id <~ "in" ~> qualifiedId

  def qualifiedId = id <~ "." ~> id | id

  def id = ident
}
