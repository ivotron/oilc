package edu.ucsc.oilc

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._,
       scalax.collection.GraphTraversal.VisitorReturn._
import scalax.collection.edge.Implicits._
import scalax.collection.io.dot._

import edu.ucsc.oilc.QueryGraph._

object HiveTranslator {
  /** Returns a query that would accomplish the mapping in Hive
   */
  def translate(sm: SchemaMapping, g: Graph[DBObject, DiEdge]) = {
    var hiveq = ""

    val sourceSubquery = generateSourceSubquery(sm)
    var first = true

    def toHiveQL(node: g.NodeT) = {
      if (node != targetNode(g) && !(node.isSuccessorOf(sourceNode(g)))) {
        if (node.container == "") {
          if (!(first)) {
            // add last fragment of previous query (if any)
            hiveq = hiveq.dropRight(2)
            hiveq += "\n  FROM\n    sub\n"
          }

          // we have a variable (a target relation)
          hiveq += "INSERT INTO TABLE " + node.name + "\n"
          hiveq += "  SELECT\n"

          first = false

        } else {
          // we have an atomic type (we assume a flat (a.k.a. relational) model)
          if (node.outNeighbors.forall {x => x.isSuccessorOf(sourceNode(g))})
            // 1. if the node is linked to a source node (directly, or via any number of equality
            //    edges), then a simple element is generated with the value equal to the expression
            //    represented by the source node
            node.outNeighbors.map { x => hiveq += "    " + x + ",\n" }

          //else if()
            // 2. if the node corresponds to an optional element, nothing is generated

          //else if()
            // 3. if the node corresponds to a nullable schema element, the null value is generated

          //else if()
            // 4. if none of the above applies, a value must be generated for that element via a
            //     Skolem function

        }
      }

      Continue
    }

    targetNode(g).traverseNodes(breadthFirst = false) {toHiveQL}
    hiveq = hiveq.dropRight(2)
    hiveq += "\n  FROM\n    sub\n"

    sourceSubquery + hiveq
  }

  def generateSourceSubquery(sm: SchemaMapping) = {
    var alreadyLookedAt = List[DBObject]()
    var sub = "FROM (\n  SELECT\n"

    def add(dbo: DBObject) = if (!(alreadyLookedAt.contains(dbo))) sub += "    " + dbo + ",\n"

    sm.foreachWhereClause.foreach { x => add(x.lhsObject); add(x.rhsObject) }

    sub = sub.dropRight(2) + "\n  FROM\n"

    sm.foreachClause.foreach { case(id, rel) => sub += "    " + rel + " AS " + id + ",\n" }

    sub = sub.dropRight(2) + "\n"

    sub += generateSourcePredicates(sm, "  ")

    sub + "\n) sub\n\n"
  }

  def generateSourcePredicates(sm: SchemaMapping, indent: String) = {
    var predicates = indent + "WHERE\n"
    sm.foreachWhereClause.foreach {
      x => predicates += (indent + indent + x.lhsObject + " = " + x.rhsObject + "AND \n")
    }
    predicates.dropRight(5)
  }
}
