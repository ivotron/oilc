package edu.ucsc.oilc

import scala.util.parsing.combinator._

/** Parses the following:
 *
 * {{{
 * FOREACH
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
 *  - variables in `FOREACH` and `EXISTS` clauses correspond to relational symbols, i.e. a variable
 *    "points" to a relation members (a.k.a. columns from tables).
 *  - `FOREACH` clause refers to relations in the source schema
 *  - `EXISTS` clause refers to relations in the target schema
 *  - equality conditions on the `WHERE` following the `FOREACH` clause refer to variables on the
 *    source schema, i.e. they're conditions over the source schema
 *  - equality conditions on the `WHERE` following the `EXISTS` clause refer to variables on the
 *    target schema, i.e. they're conditions over the target schema
 *  - conditions on the `WITH` clause relate variables used in both source and target schemas, i.e.
 *    the `FOREACH` and `EXISTS` clauses.
 *
 * Before generating a schema mapping, the parser validates the mapping semantically by checking
 * that references to source and target schemas (in `EXISTS` and `WITH` clauses, respectively) are
 * previously defined (in the `FOREACH` and `WHERE` clauses). These checks don't consider the
 * validation of the schema mapping with respect to a metadata catalog, that is, the parser doesn't
 * compare against a given catalog to see if the referenced relations actually exist.
 *
 * The result of a schema mapping parsing is a `SchemaMapping` instance.
 */
class SchemaMappingParser extends JavaTokenParsers {

  def schemaMapping =
    foreach ~ where ~ exists ~ where ~ withh ^^ {
      validate(f, fw, e, ew, w)
      case f ~ fw ~ e ~ ew ~ w => new SchemaMapping(f, fw, e, ew, w)
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

  //============
  def validate(foreach: List[RelationVariable], fwhere: List[Equality],
      exists: List[RelationVariable], ewhere: List[Equality], withh: List[Equality]) {
    // check that given expression makes semantic sense
  }
}

/** Represents an identifier, i.e. a name given to a element in a schema. This might be a column,
 * table, database or any other. Databases don't have higher-level containers. Tables are contained
 * in databases and columns are contained in tables.
 */
class Identifier(val obj: String, val container: String) {
  def this(obj: String) = this(obj, "")
}

/** An equality expression.
 */
class Equality(val id1: Identifier, val id2: Identifier) {
}

/** A relational variable, comprised of an id (the name given to the variable) and a relational
 * identifier, which in this case refers to a relation (i.e. a table)
 */
class RelationVariable(val id: String, val relation: Identifier) {
}

/** Represents a schema mapping, i.e. the result of the parsing phase. A schema mapping is comprised
 * of a list elements for each clause:
 *  - relational variables referring to the source schema.
 *  - equalities refering to the above.
 *  - relational variables referring to the target schema.
 *  - equalities refering to the both source and target schemas.
 */
class SchemaMapping(
    val foreach: List[RelationVariable],
    val fwhere: List[Equality],
    val exists: List[RelationVariable],
    val ewhere: List[Equality],
    val withh: List[Equality]) {
}
