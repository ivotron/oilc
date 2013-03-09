package edu.ucsc.oilc

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._,
       scalax.collection.GraphTraversal.VisitorReturn._
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
  val r = new DBObject("root")
  val s = new DBObject("source")
  val t = new DBObject("target")

  /**
   */
  def generateGraph (sm: SchemaMapping) = {
    var graph = Graph[DBObject, DiEdge]()

    graph.add(r)
    graph.add(r ~> s)
    graph.add(r ~> t)

    // for each relation referenced in the FOREACH and EXISTS clause we create a node
    //
    // we then create nodes for columns that are used in equality conditions and link from the
    // relation node to these newly created column-nodes
    sm.foreachClause.foreach {
      case(id, relation) =>
        graph.add(relation)
        graph.add(s ~> relation)
        addColumnToRelationEdges(graph, id, relation, sm.foreachWhereClause)
        addColumnToRelationEdges(graph, id, relation, sm.withClause)
    }
    sm.existsClause.foreach {
      case(id, relation) =>
        graph.add(relation)
        graph.add(t ~> relation)
        addColumnToRelationEdges(graph, id, relation, sm.existsWhereClause)
        addColumnToRelationEdges(graph, id, relation, sm.withClause)
    }

    // add edges between source and target columns iff they're referenced in the WITH clause
    sm.withClause.foreach {
      eqCond => addColumnToColumnEdge(graph, sm.foreachClause, sm.existsClause, eqCond)
    }

    // add edges between target columns iff they're referenced in the EXISTS' WHERE clause and they
    // refer to distinct relations. This gives us referential constraints (foreign keys)
    sm.existsWhereClause.foreach {
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
  def addColumnToRelationEdges(
      graph: Graph[DBObject, DiEdge], id: String, dbObject: DBObject, eqs: List[Equality])
  {
    eqs.foreach {
      eqCond =>
        if (id == eqCond.lhsObject.container) graph.add(dbObject ~> eqCond.lhsObject)
        if (id == eqCond.rhsObject.container) graph.add(dbObject ~> eqCond.rhsObject)
    }
  }

  /** If an equality condition relates columns from source and target (eg. c.name = o.emp_name;
   * where o is in the source or c in target (or vice-versa)), we add an edge between them
   */
  def addColumnToColumnEdge(
      graph: Graph[DBObject, DiEdge], foreachClause: Map[String,DBObject], existsClause:
      Map[String,DBObject], eq: Equality)
  {
    if ((foreachClause contains eq.lhsObject.container) &&
        (existsClause contains eq.rhsObject.container)) {
      graph.add(eq.rhsObject ~> eq.lhsObject)
    } else if ((foreachClause contains eq.rhsObject.container) &&
        (existsClause contains eq.lhsObject.container)) {
      graph.add(eq.lhsObject ~> eq.rhsObject)
    }
  }

  /** adds an edge between columns of the given equality condition, as long as they refer to
   * distinct relations. In order to determine if the expression in an equality condition refers to
   * different relations, we use a map of "id->dbobject". For example:
   *
   *  c.name = o.name
   *
   * an edge is created for the above, as long as c and o refer to distinct relations
   */
  def addColumnToColumnBidirectionalEdges(
      graph: Graph[DBObject, DiEdge], eq: Equality, map: Map[String,DBObject])
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

  def n(g: Graph[DBObject, DiEdge], n: DBObject) = g get n
  def tgt(g: Graph[DBObject, DiEdge]) = g get t
  def src(g: Graph[DBObject, DiEdge]) = g get s

  /** annotates a graph. First, every target node that is adjacent to a source schema node through
   * an “equality” (dotted) edge (target-column to source-column edge), is annotated with the
   * expression of that source schema node and only that. Next, we start propagating these
   * expressions through the rest of the graph by invoking the `propagateAnnotations`.
   */
  def annotateGraph(g: Graph[DBObject, DiEdge]) {
    (g.nodes filter (x => (x isSuccessorOf tgt(g)) && (x isSuccessorOf src(g)))).map {
      srcNode => srcNode.inNeighbors.map {
        z => if (z isSuccessorOf (n(g,t))) z.annotation += srcNode
      }
    }

    propagateAnnotations(g)
  }

  /** Applies the annotation propagation rules recursively until no more propagations are done. The
   * rules are the following:
   * – Upward propagation: Every expression that a node acquires is propagated to its parent node,
   *   unless the (aquiring) node is a variable.
   * – Downward propagation: Every expression that a node acquires is propagated to its children
   *   if they do not already have it and if they are not equal to any of the source nodes.
   * - Eq propagation: Every expression that a node acquires is propagated to the nodes related to
   *   it through equality edges.
   */
  def propagateAnnotations(g: Graph[DBObject, DiEdge]) {
    val numOfAnnotationsBefore = numberOfAnnotations(g)

    (g.nodes filter (x => (x isSuccessorOf tgt(g)) && (!(x isSuccessorOf src(g))))).map {
      tgtNode =>
        tgtNode.outNeighbors.map {
          // upward: pull from children (TODO: if a variable (w.r.t annotation), then don't pull)
          child => tgtNode.annotation ++= child.annotation
        }
        tgtNode.outNeighbors.map {
          // equality and children: propagate to children only if they're not in the source schema
          child => if (!(child isSuccessorOf (n(g,s)))) child.annotation ++= tgtNode.annotation
        }
    }

    val numOfAnnotationsAfter = numberOfAnnotations(g)

    if (numOfAnnotationsBefore != numOfAnnotationsAfter)
      propagateAnnotations(g)
  }

  /** Counts the number of annotations in the graph
   */
  def numberOfAnnotations(g: Graph[DBObject, DiEdge]) = {
    var numOfAnnotations = 0

    def count(node: g.NodeT) = {
      numOfAnnotations += node.annotation.size
      Continue
    }

    g.get(r).traverseNodes() {count}

    numOfAnnotations
  }

  /**
   * writes to dot format
   */
  def toDotGraph(g: scalax.collection.Graph[DBObject, DiEdge]): String =
  {
    val dotRoot = DotRootGraph(directed=true,id=Some("QueryGraph"))

    def edgeTransformer(innerEdge: scalax.collection.Graph[DBObject,DiEdge]#EdgeT):
        Option[(DotGraph, DotEdgeStmt)] = {
      val edge = innerEdge.edge
      val labelList = List() // edge.label.asInstanceOf[DBObject]

      Some(dotRoot, DotEdgeStmt(edge.from.toString, edge.to.toString, labelList))
    }

    g.toDot(dotRoot, edgeTransformer)
  }
}
