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
 *  - all clauses are required (this might change in the future)
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
      case f ~ fw ~ e ~ ew ~ w => validate(f, fw, e, ew, w)
    }

  def foreach =
    "foreach" ~> repsep(dbObjectVariable, ",") ^^ {
      (relVars: List[DBObjectVariable]) =>
        relVars map { relVar => (relVar.id, relVar.dbObject) } toMap
    }

  def where =
    "where" ~> repsep(equality, "and") ^^ { (eqs: List[Equality]) => eqs }

  def exists =
    "exists" ~> repsep(dbObjectVariable, ",") ^^ {
      (relVars: List[DBObjectVariable]) =>
        relVars map { relVar => (relVar.id, relVar.dbObject) } toMap
    }

  def withh =
    "with" ~> repsep(equality, "and") ^^ { (eqs: List[Equality]) => eqs }

  //===========

  def equality =
    qualifiedId ~ "=" ~ qualifiedId ^^ { case q1 ~ _ ~ q2 => new Equality(q1, q2) }

  def dbObjectVariable =
    id ~ "in" ~ qualifiedId ^^ { case i ~ _ ~ q => new DBObjectVariable(i, q) }

  def qualifiedId =
    id ~ "." ~ id ^^ { case c ~ _ ~ o => new DBObject(o, c) } |
    id            ^^ { case o => new DBObject(o) }

  def id =
    ident ^^ { case s => s }

  //============

  def validate(foreachClause: Map[String,DBObject], foreachWhereClause: List[Equality],
      existsClause: Map[String,DBObject], existsWhereClause: List[Equality], withClause: 
      List[Equality]) = {

    // can't use the same variable twice

    for (keyVal <- existsClause)
      if (foreachClause contains keyVal._1)
        throw new ParseException("alias " + keyVal._1 + " already in FOREACH clause")

    // dbobject variables used in FOREACH's WHERE clause should be declared in the FOREACH clause
    for (eq <- foreachWhereClause) {
      if (!(foreachClause contains eq.lhsObject.container))
        throw new ParseException("alias " + eq.lhsObject.container + " not in FOREACH clause")
      if (!(foreachClause contains eq.rhsObject.container))
        throw new ParseException("alias " + eq.rhsObject.container + " not in FOREACH clause")
    }

    // dbobject variables used in EXISTS's WHERE clause should be declared in the EXISTS clause
    for (eq <- existsWhereClause) {
      if (!(existsClause contains eq.lhsObject.container))
        throw new ParseException("alias " + eq.lhsObject.container + " not in EXISTS clause")
      if (!(existsClause contains eq.rhsObject.container))
        throw new ParseException("alias " + eq.rhsObject.container + " not in EXISTS clause")
    }

    // dbobject variables used in WITH clause should be declared in either FOREACH or EXISTS clause
    for (eq <- withClause) {
      if (!(foreachClause contains eq.lhsObject.container) &&
          !(existsClause contains eq.lhsObject.container))
        throw new ParseException(
            "alias " + eq.lhsObject.container + " not in FOREACH or EXISTS clauses")
      if (!(foreachClause contains eq.rhsObject.container) &&
          !(existsClause contains eq.rhsObject.container))
        throw new ParseException(
            "alias " + eq.rhsObject.container + " not in FOREACH or EXISTS clauses")
    }

    // TODO: take dbtune.metadata.Catalog as a new argument to this function and validate against it

    new SchemaMapping(
        foreachClause, foreachWhereClause,
        existsClause, existsWhereClause, withClause)
  }
}

/** Represents a database object, i.e. a name given to an element in a (metadata) catalog. This
 * might be a column, table, schema, or database. Databases don't have higher-level containers.
 * Schemas are contained inside databases, tables within schemas and columns within tables.
 */
class DBObject(val name: String, var container: String) {
  def this(obj: String) = this(obj, "")

  override def equals(other: Any) = other match {
    case that: DBObject => (that.name == this.name) && (that.container == this.container)
    case _ => false
  }

  override def hashCode = { 41 * ( 41 + name.hashCode ) + container.hashCode }

  override def toString = if (container == "") "" + name else container + "." + name
}

/** An equality expression between two database objects.
 */
class Equality(val lhsObject: DBObject, val rhsObject: DBObject) {
  def references(dbObject: DBObject): Boolean = {
    ((lhsObject == dbObject) || (rhsObject == dbObject))
  }
  // TODO: remove this and use a tuple (DBObject, DBObject)
}

/** A variable (a.k.a. reference), comprised of an id (the name given to the variable) and a
 * database object, which usually is a relation (i.e. a table) or a column
 */
class DBObjectVariable(val id: String, val dbObject: DBObject) {
  // TODO: remove this and use a tuple (String, DBObject)
}

/** Represents a schema mapping, i.e. the result of the parsing phase. A schema mapping is comprised
 * of a list elements for each clause:
 *  - relational variables referring to the source schema.
 *  - equalities refering to the above.
 *  - relational variables referring to the target schema.
 *  - equalities refering to the above.
 *  - equalities refering to the both source and target schemas.
 */
class SchemaMapping(
    val foreachClause: Map[String, DBObject],
    val foreachWhereClause: List[Equality],
    val existsClause: Map[String, DBObject],
    val existsWhereClause: List[Equality],
    val withClause: List[Equality]) {
}

class ParseException(message: String = null, cause: Throwable = null)
  extends Exception(message, cause) {
}
