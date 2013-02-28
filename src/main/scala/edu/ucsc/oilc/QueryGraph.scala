package edu.ucsc.oilc

import scalax.collection.Graph,
       scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.io.dot._

/** Given a mapping m, a graph is constructed, called the query graph. The graph will include a node
 * for each target expression that can be constructed over the variables in the exists clause of the
 * mapping. Source expressions that provide values for the target will also have nodes. Furthermore,
 * the graph will include links between nodes to reflect parent-child relationships and equality
 * predicates, respectively. This structure is then further refined to support Skolem function
 * generation; at that point, it contains all the information needed to generate the query.
 *
 * R. Fagin, L. M. Haas, M. Hernández, R. J. Miller, L. Popa, and Y. Velegrakis, “Clio: Schema
 * Mapping Creation and Data Exchange,” in Conceptual Modeling: Foundations and Applications, A. T.
 * Borgida, V. K. Chaudhri, P. Giorgini, and E. S. Yu, Eds. Springer Berlin Heidelberg, 2009, pp.
 * 198–236. http://link.springer.com/chapter/10.1007/978-3-642-02463-4_12
 *
 */
//class QueryGraph extends DefaultGraphImpl[DBObject, DiEdge](DefaultGraphImpl.Config) {
//}

object QueryGraph {
  def generateGraph (sm: SchemaMapping) = {
    var graph = scalax.collection.mutable.Graph[DBObject, DiEdge]()

    // for each relation referenced in the FOREACH and EXISTS clause we create a node
    //
    // we then create nodes for columns that are used in equality conditions and link from the
    // relation node to these newly created column-nodes
    sm.foreachClause.foreach {
      case(id, relation) =>
        graph.add(relation)
        addColumnToRelationEdges(graph, id, relation, sm.foreachWhereClause)
        addColumnToRelationEdges(graph, id, relation, sm.withClause)
    }
    sm.existsClause.foreach {
      case(id, relation) =>
        graph.add(relation)
        addColumnToRelationEdges(graph, id, relation, sm.existsWhereClause)
        addColumnToRelationEdges(graph, id, relation, sm.withClause)
    }
    sm.withClause.foreach {
      // we add edges between source and target columns iff they're referenced in the WITH clause
      eqCond => addColumnToColumnEdge(graph, sm.foreachClause, sm.existsClause, eqCond)
    }
    sm.existsWhereClause.foreach {
      // we add edges between source and target columns iff they're referenced in the WITH clause
      eqCond => addColumnToColumnBidirectionalEdges(graph, eqCond, sm.existsClause)
    }
    graph
  }

  /** we look for conditions in the given list that reference relations (nodes) in the graph, i.e.
   * those conditions whose rhs or lhs column values can be obtained by projecting the relations
   * they reference to (for an example, look in the paper refrenced above). for each attribute in an
   * equality condition that can be obtained as mentioned above (through column-projection) we
   * create an edge from the node created above, to this attribute. here, a dbObject is a parent
   * object, whereas the node in the rhs of the edge (~>) is an attribute (which in turn can be a
   * parent, if we take into account nested datamodels)
   */
  def addColumnToRelationEdges(graph: scalax.collection.mutable.Graph[DBObject, DiEdge],
      id: String, dbObject: DBObject, eqs: List[Equality]) {
    eqs.foreach {
      eqCond =>
        if (id == eqCond.lhsObject.container) graph.add(dbObject ~> eqCond.lhsObject)
        if (id == eqCond.rhsObject.container) graph.add(dbObject ~> eqCond.rhsObject)
    }
  }

  /** If an equality condition relates columns from source and target (eg. c.name = o.emp_name;
   * where o is in the source or c in target (or vice-versa)), we add an edge between them
   */
  def addColumnToColumnEdge(graph: scalax.collection.mutable.Graph[DBObject, DiEdge], foreachClause:
      Map[String,DBObject], existsClause: Map[String,DBObject], eq: Equality) {
    if ((foreachClause contains eq.lhsObject.container) &&
        (existsClause contains eq.rhsObject.container)) {
      graph.add(eq.rhsObject ~> eq.lhsObject)
    } else if ((foreachClause contains eq.rhsObject.container) &&
        (existsClause contains eq.lhsObject.container)) {
      graph.add(eq.lhsObject ~> eq.rhsObject)
    }
  }

  /**
   * adds an edge between columns of the given equality condition, as long as they refer to distinct 
   * relations. In order to determine if the expression in an equality condition refers to different 
   * relations, we use a map of "id->dbobject". For example:
   *
   *  c.name = o.name
   *
   * an edge is created for the above, as long as c and o refer to distinct relations
   */
  def addColumnToColumnBidirectionalEdges(graph: scalax.collection.mutable.Graph[DBObject, DiEdge], 
      eq: Equality, map: Map[String,DBObject])
  {
    (map.get(eq.lhsObject.container), map.get(eq.rhsObject.container)) match {
      case (Some(lhsObject), Some(rhsObject)) =>
        if (lhsObject.name != rhsObject.name) {
          graph.add(eq.lhsObject ~> eq.rhsObject)
          graph.add(eq.rhsObject ~> eq.lhsObject)
        }
      case (_,_) =>
        throw new RuntimeException("Missing keys")
     }
  }

  def toDotGraph(graph: Graph[DBObject, DiEdge]): String = {
    val root = DotRootGraph(directed=true,id=Some("QueryGraph"),strict=true)

    def edgeTransformer(innerEdge: Graph[DBObject,DiEdge]#EdgeT):
        Option[(DotGraph, DotEdgeStmt)] = {
      val edge = innerEdge.edge
      val labelList = List(DotAttr("l", " ")) // edge.label.asInstanceOf[DBObject]

      Some(root, DotEdgeStmt(edge.from.toString, edge.to.toString, labelList))
    }

    def iNodeTransformer(innerNode: Graph[DBObject,DiEdge]#NodeT):
        Option[(DotGraph, DotNodeStmt)] = {
      println("transforming node " + innerNode.toString)
      Some(root, DotNodeStmt(innerNode.toString, Seq.empty[DotAttr]))
    }

    graph.toDot(root, edgeTransformer, Some(iNodeTransformer))
  }
}
