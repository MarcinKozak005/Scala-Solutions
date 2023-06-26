package P03_Intermediate

import P03_Intermediate.Graph.{Edge, Vertex}

import scala.annotation.tailrec

class P03Graph extends App {
}

/**
 * Multipurpose immutable Graph class
 *
 * Can serve as:
 *
 *  - [[https://en.wikipedia.org/wiki/Graph_(discrete_mathematics) undirected graph]]
 *  - [[https://en.wikipedia.org/wiki/Directed_graph directed graph]]
 *  - [[https://en.wikipedia.org/wiki/Mixed_graph mixed graph]]
 *  - [[https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)#Weighted_graph weighted graph]]
 *  - [[https://en.wikipedia.org/wiki/Multigraph multigraph]]
 * */
class Graph private(vertices: Map[Vertex, Option[Int]],
                    edges: Map[Edge, Option[Int]],
                    inEdges: Map[Vertex, Set[Edge]],
                    outEdges: Map[Vertex, Set[Edge]],
                    toVertices: Map[Edge, Set[Vertex]],
                    fromVertices: Map[Edge, Set[Vertex]],
                   ) {

  /** Creates a new graph obtained by updating this graph with a given vertex/value pair. */
  def setVertex(name: Vertex, value: Option[Int] = None): Graph =
    Graph(
      vertices.updated(name, value),
      edges,
      inEdges.updated(name, Set[Edge]()),
      outEdges.updated(name, Set[Edge]()),
      toVertices,
      fromVertices,
    )

  def getVertexValue(name: Vertex): Option[Int] = vertices.getOrElse(name, None)

  /** Removes a vertex from this graph, returning a new graph. */
  def removeVertex(name: Vertex): Graph =
    Graph(
      vertices.removed(name),
      edges,
      inEdges.removed(name),
      outEdges.removed(name),
      toVertices,
      fromVertices,
    )

  /** Creates a new graph with directed edge between specified vertices. Adds vertices if they don't exist. */
  def setDirectedConnection(from: Vertex, to: Vertex, edge: Edge, value: Option[Int] = None): Graph =
    addDirectedConnectionHelper(from, to, s"d_$edge", value)

  /** Creates a new graph with undirected edge between specified vertices. Adds vertices if they don't exist. */
  def setUndirectedConnection(v1: Vertex, v2: Vertex, edge: Edge, value: Option[Int] = None): Graph =
    this
      .addDirectedConnectionHelper(v1, v2, s"u_$edge", value)
      .addDirectedConnectionHelper(v2, v1, s"u_$edge", value)

  /** Removes an edge from this graph, returning a new graph. */
  def removeEdge(edge: Edge): Graph = Graph(
    vertices,
    edges.removed(edge),
    removeEdgeFromVertices(inEdges, toVertices.getOrElse(edge, Set[Vertex]()).toList, edge),
    removeEdgeFromVertices(outEdges, fromVertices.getOrElse(edge, Set[Vertex]()).toList, edge),
    toVertices.removed(edge),
    fromVertices.removed(edge),
  )

  @tailrec
  private def removeEdgeFromVertices(collection: Map[Vertex, Set[Edge]],
                                     verticesList: List[Vertex],
                                     edge: Edge
                                    ): Map[Vertex, Set[Edge]] =
    verticesList match {
      case v :: tail => removeEdgeFromVertices(
        collection.updated(v, collection.getOrElse(v, Set[Edge]()).excl(edge)),
        tail,
        edge,
      )
      case Nil => collection
    }

  private def addDirectedConnectionHelper(from: Vertex, to: Vertex, edge: Edge, value: Option[Int]): Graph =
    this
      .setVertex(from, getVertexValue(from))
      .setVertex(to, getVertexValue(to))
      .addOutEdge(from, edge, value)
      .addInEdge(to, edge, value)

  private def addInEdge(to: Vertex, edge: Edge, value: Option[Int] = None) = Graph(
    vertices,
    edges.updated(edge, value),
    inEdges.updated(to, inEdges.getOrElse(to, Set()) + edge),
    outEdges,
    toVertices.updated(edge, toVertices.getOrElse(edge, Set()) + edge),
    fromVertices,
  )

  private def addOutEdge(from: Vertex, edge: Edge, value: Option[Int] = None) = Graph(
    vertices,
    edges.updated(edge, value),
    inEdges,
    outEdges.updated(from, outEdges.getOrElse(from, Set()) + from),
    toVertices,
    fromVertices.updated(edge, fromVertices.getOrElse(edge, Set()) + from),
  )

}

object Graph {
  type Vertex = String
  type Edge = String

  def apply() =
    new Graph(
      Map[Vertex, Option[Int]](),
      Map[Edge, Option[Int]](),
      Map[Vertex, Set[Edge]](),
      Map[Vertex, Set[Edge]](),
      Map[Edge, Set[Vertex]](),
      Map[Edge, Set[Vertex]](),
    )

  private def apply(vertices: Map[Vertex, Option[Int]],
                    edges: Map[Edge, Option[Int]],
                    inEdges: Map[Vertex, Set[Edge]],
                    outEdges: Map[Vertex, Set[Edge]],
                    toVertices: Map[Edge, Set[Vertex]],
                    fromVertices: Map[Edge, Set[Vertex]]) =
    new Graph(vertices, edges, inEdges, outEdges, toVertices, fromVertices)
}
